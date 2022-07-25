module Listasio.Page.Dashboard where

import Prelude

import Data.Array (null, snoc)
import Data.Either (Either, note)
import Data.Filterable (filter)
import Data.Lens (_Just, preview)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.MediaType.Common as MediaType
import Data.Traversable (for_, traverse)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HES
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Html.Renderer.Halogen as RH
import Listasio.Capability.Clipboard (class Clipboard)
import Listasio.Capability.Navigate (class Navigate, navigate_)
import Listasio.Capability.Now (class Now)
import Listasio.Capability.Resource.List (class ManageList, getLists)
import Listasio.Capability.Resource.Resource (class ManageResource, getResource)
import Listasio.Component.HTML.CreateResource as CreateResource
import Listasio.Component.HTML.EditResource as EditResource
import Listasio.Component.HTML.List as List
import Listasio.Component.HTML.Modal as Modal
import Listasio.Component.HTML.Utils (maybeElem, safeHref, whenElem)
import Listasio.Data.ID (ID)
import Listasio.Data.Lens (_id, _lists, _next, _resource_metadata)
import Listasio.Data.List (ListWithIdUserAndMeta)
import Listasio.Data.Profile (ProfileWithIdAndEmail)
import Listasio.Data.Resource (ListResource)
import Listasio.Data.Route (Route(..))
import Listasio.Store as Store
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import Tailwind as T
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Util as Util
import Web.Clipboard.ClipboardEvent (ClipboardEvent, clipboardData) as Clipboard
import Web.Clipboard.ClipboardEvent.EventTypes (paste) as Clipboard
import Web.Event.Event (Event)
import Web.HTML (window) as Web
import Web.HTML.Event.DataTransfer as DataTransfer
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.MouseEvent as Mouse

_slot :: Proxy "dashboard"
_slot = Proxy

type Lists = RemoteData String (Array ListWithIdUserAndMeta)

type StoreState
  =
  { currentUser :: Maybe ProfileWithIdAndEmail
  , lists :: Lists
  , reader :: Maybe Store.ReaderState
  }

data Action
  = Initialize
  | Receive (Connected StoreState Unit)
  | LoadLists
  | PasteUrl Clipboard.ClipboardEvent
  | ToggleAddResource
  | ResourceAdded CreateResource.Output
  | HandleListOutput List.Output
  | CloseEditModal
  | ResourceEdited EditResource.Output
  -- Reader
  | GetReaderResource
  -- meta actions
  | Navigate Route Event
  | NoOp

type State
  =
  { currentUser :: Maybe ProfileWithIdAndEmail
  , lists :: Lists
  , showAddResource :: Boolean
  , showEditResource :: Maybe ListResource
  , pastedUrl :: Maybe String
  , listToAddResource :: Maybe ID
  , reader :: Maybe Store.ReaderState
  }

type ChildSlots
  =
  ( createResource :: CreateResource.Slot
  , editResource :: EditResource.Slot
  , list :: List.Slot
  )

noteError :: forall a. Maybe a -> Either String a
noteError = note "Could not fetch your lists"

select :: Store.Store -> StoreState
select { lists, currentUser, reader } = { lists, currentUser, reader }

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageList m
  => ManageResource m
  => Navigate m
  => Clipboard m
  => Now m
  => H.Component q Unit o m
component = connect (selectEq select) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  initialState { context: { currentUser, lists, reader } } =
    { currentUser
    , lists
    , showAddResource: false
    , showEditResource: Nothing
    , pastedUrl: Nothing
    , listToAddResource: Nothing
    , reader
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork $ handleAction LoadLists
      document <- H.liftEffect $ HTMLDocument.toEventTarget <$> (Web.document =<< Web.window)
      -- unsafeCoerce is fine here, we are only listening to clipboard paste events :)
      -- Halogen does the same ;)
      -- https://github.com/purescript-halogen/purescript-halogen/blob/2f8531168207cda5256dc64da60f791afe3855dc/src/Halogen/HTML/Events.purs#L271-L272
      -- https://github.com/purescript-halogen/purescript-halogen/blob/2f8531168207cda5256dc64da60f791afe3855dc/src/Halogen/HTML/Events.purs#L151-L152
      void $ H.subscribe $ HES.eventListener Clipboard.paste document (Just <<< PasteUrl <<< unsafeCoerce)
      handleAction GetReaderResource

    Receive { context: { lists, currentUser, reader } } -> do
      H.modify_ _ { currentUser = currentUser, lists = lists, reader = reader }
      case lists, reader of
        Success _, Nothing -> handleAction GetReaderResource
        _, _ -> pure unit

    LoadLists -> do
      { lists } <- H.get
      when (RemoteData.isNotAsked lists) $ updateStore $ Store.SetLists Loading
      result <- RemoteData.fromEither <$> noteError <$> getLists
      updateStore $ Store.SetLists result

    ResourceAdded (CreateResource.Created resource) -> do
      H.tell List._slot resource.list $ List.ResourceAdded resource
      H.modify_ _ { showAddResource = false, pastedUrl = Nothing }

    HandleListOutput (List.CreateResourceForList id) -> do
      { showEditResource } <- H.get
      when (isNothing showEditResource) do H.modify_ _ { listToAddResource = Just id, showAddResource = true }

    HandleListOutput (List.EditResource resource) -> do
      { showAddResource } <- H.get
      when (not showAddResource) do H.modify_ _ { showEditResource = Just resource }

    CloseEditModal -> H.modify_ _ { showEditResource = Nothing }

    ResourceEdited (EditResource.Updated update@{ old, new }) -> do
      H.tell List._slot old.list $ List.ResourceEdited update
      when (old.list /= new.list) do
        H.tell List._slot old.list $ List.ResourceEdited update
      H.modify_ _ { showEditResource = Nothing }

    ToggleAddResource -> do
      { showEditResource } <- H.get
      when (isNothing showEditResource) do
        H.modify_ \s -> s
          { showAddResource = not s.showAddResource
          , pastedUrl = filter (const $ not s.showAddResource) s.pastedUrl
          , listToAddResource = Nothing
          }

    PasteUrl event -> do
      { showAddResource, lists, showEditResource } <- H.get
      when (not showAddResource && isNothing showEditResource && RemoteData.isSuccess lists) do
        mbUrl <- H.liftEffect $ filter Util.isUrl <$> traverse (DataTransfer.getData MediaType.textPlain) (Clipboard.clipboardData event)
        for_ mbUrl \url -> H.modify_ _ { showAddResource = true, pastedUrl = Just url }

    -- Reader
    GetReaderResource -> do
      resourceToRead <- H.gets $ preview (_lists <<< _Success <<< ix 0 <<< _resource_metadata <<< _next <<< _Just <<< _id)
      for_ resourceToRead \id -> do
        mbResource <- getResource id
        for_ mbResource $ updateStore <<< Store.SetReader

    -- Meta

    Navigate route e -> navigate_ e route

    NoOp -> pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render st =
    HH.div
      []
      [ header
      , HH.div [ HP.classes [ T.container ] ] [ feed ]
      ]

    where
    header =
      HH.div
        [ HP.classes [ T.flex, T.justifyBetween, T.wFull, T.pt2 ] ]
        [ HH.h1
            [ HP.classes [ T.textGray400, T.mb6, T.text4xl, T.fontBold ] ]
            [ HH.text "Up next" ]
        , maybeElem (filter (not <<< null) $ RemoteData.toMaybe st.lists) \_ ->
            HH.div
              []
              [ HH.button
                  [ HE.onClick \_ ->
                      if st.showAddResource then NoOp
                      else ToggleAddResource
                  , HP.classes
                      [ T.flexNone
                      , T.cursorPointer
                      , T.leadingNormal
                      , T.py2
                      , T.px4
                      , T.textWhite
                      , T.roundedMd
                      , T.bgKiwi
                      , T.hoverBgKiwiDark
                      , T.focusBgKiwiDark
                      , T.focusRing2
                      , T.focusRingKiwiDark
                      , T.focusRingOffset2
                      , T.focusOutlineNone
                      ]
                  ]
                  [ HH.text "Add Resource" ]
              ]
        ]

    feed = case st.lists of
      Success lists ->
        HH.div
          []
          [ HH.div
              [ HP.classes [ T.grid, T.gridCols1, T.mdGridCols3, T.gap4, T.itemsStart ] ]
              [ HH.div
                  [ HP.classes [ T.colSpan2, T.p8, T.bgWhite, T.roundedLg ] ]
                  [ case st.reader of
                      Just {resource: {html: Just rawHtml}} -> HH.div [ HP.classes [ T.rawHtml ] ] [ RH.render_ rawHtml ]
                      _ -> HH.div [] [ HH.text "No article ..." ]
                  ]
              , HH.div
                  [ HP.classes [ T.grid, T.gap4, T.colSpan1 ] ]
                  $ snoc
                      ( map
                          (\list -> HH.slot List._slot list.id List.component { list } HandleListOutput)
                          lists
                      )
                      listCreateCard
              ]
          , Modal.modal st.showAddResource ({ onClose: ToggleAddResource, noOp: NoOp }) $
              HH.div
                []
                [ HH.div
                    [ HP.classes [ T.textCenter, T.textGray400, T.text2xl, T.fontBold, T.mb4 ] ]
                    [ HH.text "Add new resource" ]
                , whenElem st.showAddResource \_ ->
                    let
                      input = { lists, url: st.pastedUrl, selectedList: st.listToAddResource, text: Nothing, title: Nothing }
                    in
                      HH.slot CreateResource._slot unit CreateResource.component input ResourceAdded
                ]
          , Modal.modal (isJust st.showEditResource) ({ onClose: CloseEditModal, noOp: NoOp }) $
              HH.div
                []
                [ HH.div
                    [ HP.classes [ T.textCenter, T.textGray400, T.text2xl, T.fontBold, T.mb4 ] ]
                    [ HH.text "Edit resource" ]
                , maybeElem st.showEditResource \resource ->
                    HH.slot EditResource._slot unit EditResource.component { lists, resource } ResourceEdited
                ]
          ]

      Failure msg ->
        HH.div
          [ HP.classes [ T.p4, T.border4, T.borderRed600, T.bgRed200, T.textRed900 ] ]
          [ HH.p [ HP.classes [ T.fontBold, T.textLg ] ] [ HH.text "Error =(" ]
          , HH.p_ [ HH.text msg ]
          ]

      _ -> HH.div [ HP.classes [ T.textCenter ] ] [ HH.text "Loading ..." ]

    listCreateCard =
      HH.a
        [ safeHref CreateList
        , HE.onClick $ Navigate CreateList <<< Mouse.toEvent
        , HP.classes [ T.border2, T.borderKiwi, T.roundedMd, T.flex, T.itemsCenter, T.justifyCenter, T.p8, T.bgWhite, T.h56 ]
        ]
        [ HH.div
            [ HP.classes
                [ T.cursorPointer
                , T.flex
                , T.flexCol
                , T.justifyCenter
                , T.itemsCenter
                ]
            ]
            [ HH.span [ HP.classes [ T.text7xl, T.textKiwi, T.leadingNone ] ] [ HH.text "+" ]
            , HH.span [ HP.classes [ T.textGray300 ] ] [ HH.text "Create new list" ]
            ]
        ]

-- rawHtml :: String
-- rawHtml =
--   """
--   <div id="readability-page-1" class="page"><div id="site-main"> <article> <figure> <picture> <source media="(max-width: 700px)" sizes="1px" srcset="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7 1w"> <source media="(min-width: 701px)" sizes="(max-width: 800px) 400px, (max-width: 1170px) 700px, 1400px" srcset="https://cdn-media-1.freecodecamp.org/images/1*omoLHRzItrE69MC6Um6pIA.png 300w, https://cdn-media-1.freecodecamp.org/images/1*omoLHRzItrE69MC6Um6pIA.png 600w, https://cdn-media-1.freecodecamp.org/images/1*omoLHRzItrE69MC6Um6pIA.png 1000w, https://cdn-media-1.freecodecamp.org/images/1*omoLHRzItrE69MC6Um6pIA.png 2000w"> <img onerror="this.style.display='none'" src="https://cdn-media-1.freecodecamp.org/images/1*omoLHRzItrE69MC6Um6pIA.png" alt="If you’re a developer transitioning into data science, here are your best resources"> </picture> </figure> <section> <div> <p>by Cecelia Shao</p><p>It seems like everyone wants to be a data scientist these days — from PhD students to data analysts to your old college roommate who keeps Linkedin messaging you to ‘grab coffee’.</p><p>Perhaps you’ve had the same inkling that you should at least explore some data science positions and see what the hype is about. Maybe you’ve seen articles like Vicki Boykis’ <a href="https://veekaybee.github.io/2019/02/13/data-science-is-different" rel="noopener">Data Science is different now</a> that states:</p><blockquote><strong>What is becoming clear is that, in the late stage of the hype cycle, data science is asymptotically moving closer to engineering, and the <a href="https://www.youtube.com/watch?v=frQeK8xo9Ls" rel="noopener">skills that data scientists need</a> moving forward are less visualization and statistics-based, and <a href="https://tech.trivago.com/2018/12/03/teardown-rebuild-migrating-from-hive-to-pyspark/" rel="noopener">more in line with traditional computer science</a>…:</strong></blockquote><blockquote>Concepts like unit testing and continuous integration rapidly found its way into the jargon and the toolset commonly used by data scientist and numerical scientist working on ML engineering.</blockquote><p>or <a href="https://twitter.com/tdhopper/status/730425632862044161" rel="noopener">tweets</a> like Tim Hopper’s:</p><p>What’s not clear is how you can leverage your experience as a software engineer into a data science position. Some other questions you might have are:</p><p><em>What should I prioritize learning?</em></p><p><em>Are there best practices or tools that are different for data scientists?</em></p><p><em>Will my current skill set carry over to a data science role?</em></p><p>This article will provide a background on the data scientist role and why your background might be a good fit for data science, plus tangible stepwise actions that you, as a developer, can take to ramp up on data science.</p><blockquote>Want to see the latest data science roles? Subscribe to the biweekly <a href="https://www.getrevue.co/profile/mljobs" rel="noopener">ML Jobs Newsletter</a> for new data science job openings in your inbox.</blockquote><h3 id="data-scientist-versus-data-engineer">Data Scientist versus Data Engineer</h3><p>First things first, we should distinguish between two complementary roles: Data Scientist versus Data Engineer. While both of these roles handle machine learning models, their interaction with these models as well as the the requirements and nature of the work for Data Scientists and Data Engineers vary widely.</p><blockquote>Note: The Data Engineer role that is specialized for machine learning can also manifest itself in job descriptions as ‘Software Engineer, Machine Learning’ or ‘Machine Learning Engineers’</blockquote><p>As part of <a href="https://skymind.ai/wiki/machine-learning-workflow" rel="noopener">a machine learning workflow</a>, data scientist will perform the statistical analysis required to determine which machine learning approach to use then begin prototyping and building out those models.</p><p>Machine learning engineers will often collaborate with data scientists before and after this modeling process: (1) building data pipelines to feed data into these models and (2) design an engineering system that will serve these models to ensure continuous model health.</p><p>The diagram below is one way to view this continuum of skills:</p><figure><img src="https://cdn-media-1.freecodecamp.org/images/1*3u1RTgYVDpQHvLcbQVYT4g.png" alt=""></figure><p>There is a wealth of online resources on the difference between Data Scientists and Data Engineers — make sure to check out:</p><ul><li><a href="https://blog.panoply.io/what-is-the-difference-between-a-data-engineer-and-a-data-scientist" rel="noopener">Panoply: What is the difference between a data engineer and a data scientist?</a></li><li><a href="https://www.springboard.com/blog/machine-learning-engineer-vs-data-scientist/" rel="noopener">Springboard: Machine Learning Engineer vs Data Scientist</a></li><li><a href="https://www.oreilly.com/ideas/data-engineers-vs-data-scientists" rel="noopener">O’Reilly: Data engineers vs. data scientists</a></li></ul><p>As a disclaimer, this article primarily covers the Data Scientist role with some nod towards the Machine Learning Engineering side (especially relevant if you’re looking at position in a smaller company where you might have to serve as both). If you’re interested in seeing how you can transition to being a Data Engineer or Machine Learning Engineer, let us know in the comments below!</p><h3 id="your-advantage-as-a-developer">Your advantage as a developer</h3><p>To everyone’s detriment, classes around machine learning like ‘Introduction to Data Science in Python’ or Andrew Ng’s Coursera course do <em>not</em> cover concepts and best practices from software engineering like unit testing, writing modular reusable code, CI/CD, or version control. Even some of the most advanced machine learning teams still do not use these practices for their machine learning code, leading to a disturbing trend…</p><p>Pete Warden described this trend as ‘<a href="https://petewarden.com/2018/03/19/the-machine-learning-reproducibility-crisis/" rel="noopener">the Machine Learning Reproducibility Crisis</a>’:</p><blockquote>we’re still back in the dark ages when it comes to tracking changes and rebuilding models from scratch.<strong> It’s so bad it sometimes feels like stepping back in time to when we coded without source control.</strong></blockquote><p>While you may not see these ‘software engineering’ skills explicitly stated in data scientist job descriptions, having a good grasp of these skills as part of your background already will help 10x your work as a data scientist. Plus they’ll come into use when it’s time to answer those programming questions during your data science interview.</p><p>For some interesting perspective from the other side, check out <a href="https://www.freecodecamp.org/news/if-youre-a-developer-transitioning-into-data-science-here-are-your-best-resources-c31928b53cd1/undefined" rel="noopener">Trey Causey</a>’s piece on ‘<a href="http://treycausey.com/software_dev_skills.html" rel="noopener">Software development skills for data scientists</a>’ on skills that he recommends data scientists should learn to “write better code, interact better with software developers, and ultimately save you time and headaches”.</p><h3 id="ramping-up-on-data-science">Ramping up on data science</h3><p>It’s great that you have a good foundation with your software engineering background, but what’s the next step towards becoming a data scientist? Josh Will’s tongue-in-cheek tweet on the definition of a data scientist is surprisingly accurate:</p><p>It hints at one of the topics you should catch up on if you’re interested in pursuing a data scientist role or career: statistics. In this next section, we’ll cover great resources for:</p><ul><li><strong>Building ML-specific knowledge</strong></li><li><strong>Building industry knowledge</strong></li><li><strong>Tools in the ML stack</strong></li><li><strong>Skills and qualifications</strong></li></ul><h4 id="building-ml-specific-knowledge"><strong>Building ML-specific knowledge</strong></h4><p>It’s most effective to build a combination of theory-based knowledge around probability and statistics as well as applied skills in things like data wrangling or training models on GPUs/distributed compute.</p><p>One way to frame the knowledge you’re gaining is to reference it against the machine learning workflow.</p><figure><img src="https://cdn-media-1.freecodecamp.org/images/1*omoLHRzItrE69MC6Um6pIA.png" alt=""><figcaption>A simplified view of the machine learning workflow</figcaption></figure><blockquote>See <a href="https://skymind.ai/wiki/machine-learning-workflow" rel="noopener">this detailed workflow</a> from Skymind AI</blockquote><p>Here we list out some of the best resources you can find around machine learning. It would be impossible to have an exhaustive list and to save space (and reading time) we didn’t mention very popular resources like Andrew Ng’s Coursera course or Kaggle.</p><p><strong>Courses:</strong></p><ul><li><a href="https://www.fast.ai/" rel="noopener">Fast.ai MOOC</a> (free courses that teach very applied skills across Practical Deep Learning for Coders, Cutting Edge Deep Learning for Coders, Computational Linear Algebra, and Introduction to Machine Learning for Coders)</li><li>Khan Academy</li><li><a href="https://www.youtube.com/channel/UCYO_jab_esuFRV4b17AJtAw" rel="noopener">3Blue1Brown</a> and <a href="https://www.youtube.com/channel/UCcAtD_VYwcYwVbTdvArsm7w" rel="noopener">mathematicalmonk</a> youtube channel</li><li>Udacity courses (including <a href="https://www.datacamp.com/courses/preprocessing-for-machine-learning-in-python" rel="noopener">Preprocessing for Machine Learning in Python</a>)</li><li><a href="https://www.springboard.com/blog/ai-machine-learning-career-track/" rel="noopener">Springboard AI/ML-specific</a> track</li></ul><p><strong>Textbooks: *</strong><em>tried to find free PDFs online for most of these*</em></p><ul><li><a href="http://camdavidsonpilon.github.io/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/" rel="noopener">Probabilistic Programming &amp; Bayesian Methods for Hackers</a></li><li><a href="https://www.amazon.com/Probability-Random-Processes-Geoffrey-Grimmett/dp/0198572220/" rel="noopener">Probability and Random Processes</a></li><li><a href="https://web.stanford.edu/~hastie/Papers/ESLII.pdf" rel="noopener">Elements of Statistic Learning</a></li><li><a href="http://148.206.53.84/tesiuami/S_pdfs/Linear%20Algebra%20Done%20Right.pdf" rel="noopener">Linear Algebra Done Right</a></li><li><a href="http://math.mit.edu/~gs/linearalgebra/" rel="noopener">Introduction to Linear Algebra</a></li><li><a href="http://www.cs.sjtu.edu.cn/~jiangli/teaching/CS222/files/materials/Algorithm%20Design.pdf" rel="noopener">Algorithm Design</a></li></ul><p><strong>Guides:</strong></p><ul><li><a href="https://developers.google.com/machine-learning/guides/rules-of-ml/" rel="noopener">Google Developers Machine Learning Guide</a></li><li><a href="https://machinelearningmastery.com/start-here/" rel="noopener">Machine Learning Mastery Guides</a> (for a good starting point, see <a href="https://machinelearningmastery.com/python-machine-learning-mini-course/" rel="noopener">this mini course on Python Machine Learning</a>)</li><li><a href="https://www.pyimagesearch.com/" rel="noopener">Pyimagesearch</a> (for computer vision)</li></ul><p><strong>Meetups:</strong><em> *primarily NYC-based ones*</em></p><ul><li><a href="https://paperswelove.org/" rel="noopener">Papers We Love</a></li><li><a href="https://www.meetup.com/NYC-Artificial-Intelligence-Machine-Learning/" rel="noopener">NYC Artificial Intelligence &amp; Machine Learning</a></li><li><a href="https://www.meetup.com/DataCouncil-AI-NYC-Data-Engineering-Science/" rel="noopener">DataCouncil.ai</a></li><li><a href="https://www.meetup.com/NYAImeetup/" rel="noopener">NY Artificial Intelligence</a></li></ul><blockquote>For a cool starting point, check out Will Wolf’s ‘<a href="http://willwolf.io/2018/02/03/practical-guide-open-source-ml-masters/" rel="noopener">Open-Source Machine Learning Masters’</a> on how you can structure your time across studying specific topics and working on projects to showcase expertise in a low-cost remote location.</blockquote><h4 id="building-industry-specific-knowledge">Building industry-specific knowledge</h4><p>If you have an inkling that you would like to be a specific industry like healthcare, financial services, consumer goods, retail, etc…, it is invaluable to catch up on the pain points and developments of that industry as it relates to data and machine learning.</p><p><strong>One pro tip = </strong>you can scan the websites of vertical-specific AI startups and see how they’re positioning their value proposition and where machine learning comes into play. This will give you ideas for specific areas of machine learning to study and topics for projects to showcase your work.</p><p><strong>We can walk through an example: </strong>let’s say I’m interested in working in healthcare.</p><ol><li>Through a quick google search for “<em>machine learning healthcare”, </em>I found this list from Healthcareweekly.com on ‘<a href="https://healthcareweekly.com/best-healthcare-startups-to-watch-for-in-2019/" rel="noopener">Best Healthcare Startups to Watch for in 2019</a>’</li></ol><blockquote>You can also do quick searches on <a href="https://www.crunchbase.com/hub/health-care-startups#section-leaderboard" rel="noopener">Crunchbase</a> or <a href="https://angel.co/jobs#find/f!%7B%22keywords%22%3A%5B%22Healthcare%22%5D%7D" rel="noopener">AngelList</a> with “healthcare” as a keyword</blockquote><p>2. Let’s take one of the companies featured on the list, <a href="https://benevolent.ai/" rel="noopener">BenevolentAI</a>, as an example.</p><figure><img src="https://cdn-media-1.freecodecamp.org/images/1*PfrEeqvUlERnSvGVdoBDWg.png" alt=""></figure><p>3. BenevolentAI’s website states:</p><blockquote>We are an AI company with end-to-end capability from early drug discovery to late-stage clinical development. BenevolentAI combines the power of computational medicine and advanced AI with the principles of open systems and cloud computing to transform the way medicines are designed, developed, tested and brought to market.</blockquote><blockquote>We built the Benevolent Platform to better understand disease and to design new, and improve existing treatments, from vast quantities of biomedical information. We believe our technology empowers scientists to develop medicines faster and more cost-efficiently.</blockquote><blockquote>A new research paper is published every 30 seconds yet scientists currently only use a fraction of the knowledge available to understand the cause of disease and propose new treatments. Our platform ingests, ‘reads’ and contextualises vast quantities of information drawn from written documents, databases and experimental results. It is able to make infinitely more deductions and inferences across these disparate, complex data sources, identifying and creating relationships, trends and patterns, that would be impossible for a human being to make alone.</blockquote><p>4. Immediately you can see that BenevolentAI is using natural language processing (NLP) and are probably working with some knowledge graphs if they’re identifying relationships between diseases and treatment research</p><p>5. If you check BenevolentAI’s career page, you can see that they’re hiring for a <a href="https://benevolent.ai/career-open-positions/senior-machine-learning-researcher" rel="noopener">Senior Machine Learning Researcher</a>. This is a senior role, so it’s not a perfect example, but take a look at the skills and qualifications they’re asking for below:</p><p><strong>Note:</strong></p><ul><li>natural language processing, knowledge graph inference, active learning and biochemical modeling</li><li>structured and unstructured data sources</li><li>bayesian model approaches</li><li>knowledge of modern tools for ML</li></ul><figure><img src="https://cdn-media-1.freecodecamp.org/images/1*LXgNRqLT8u28wc86N1OSrw.png" alt=""></figure><p><strong>This should give you some steps for what to approach next:</strong></p><ul><li>working with structured data</li><li>working with unstructured data</li><li>classifying relationships in knowledge graphs (see a good resource <a href="https://medium.com/comet-ml/using-fasttext-and-comet-ml-to-classify-relationships-in-knowledge-graphs-e73d27b40d67" rel="noopener">here</a>)</li><li>learning bayesian probability and modeling approaches</li><li>work on an NLP project (so text data)</li></ul><p>We’re not recommending that you apply to the companies you find through your search, but rather see how they describe their customer’s pain points, their company’s value propositions, and what kind of skills they list in their job descriptions to guide your research.</p><h4 id="tools-in-the-ml-stack">Tools in the ML stack</h4><p>In the BenevolentAI Senior Machine Learning Researcher job description, they ask for <em>“knowledge of modern tools for ML, such as Tensorflow, PyTorch, etc…”</em></p><p>Learning these modern tools for ML can seem daunting since the space is always changing. To break up the learning process into manageable pieces, remember to anchor your thinking around the machine learning workflow from above — <em>“What tool can help me with this part of the workflow?” </em>?</p><figure><img src="https://cdn-media-1.freecodecamp.org/images/1*omoLHRzItrE69MC6Um6pIA.png" alt=""></figure><p>To see which tools accompany each step of this machine learning workflow, check out <a href="https://www.freecodecamp.org/news/if-youre-a-developer-transitioning-into-data-science-here-are-your-best-resources-c31928b53cd1/undefined" rel="noopener">Roger Huang</a>’s ‘<a href="https://hackernoon.com/introduction-to-the-machine-learning-stack-f5b64bba7602" rel="noopener">Introduction to the Machine Learning Stack</a>’ which covers tools like <a href="https://www.docker.com/" rel="noopener">Docker</a>, <a href="http://www.comet.ml/" rel="noopener">Comet.ml</a>, and <a href="https://dask-ml.readthedocs.io/en/latest/" rel="noopener">dask-ml</a>.</p><p>Tactically speaking, <a href="https://www.python.org/" rel="noopener">Python</a> and <a href="https://www.r-project.org/about.html" rel="noopener">R</a> are the most common programming languages data scientists use and you can will encounter add-on packages designed for data science applications, such as <a href="http://www.numpy.org/" rel="noopener">NumPy</a> and <a href="http://www.scipy.org/" rel="noopener">SciPy</a>, and matplotlib. These languages are interpreted, rather than compiled, leaving the data scientist free to focus on the problem rather than nuances of the language. It’s worth investing time learning object-oriented programming to understand the implementation of data structures as classes.</p><p>To catch up on ML frameworks like Tensorflow, Keras, and PyTorch, make sure to go to their documentation and try implementing their tutorials end-to-end.</p><p>At the end of the day, you want to make sure that you’re building out projects that showcase these modern tools for data collection and wrangling, machine learning experiment management, and modeling.</p><p>For some inspiration for your projects, check out <a href="https://www.freecodecamp.org/news/if-youre-a-developer-transitioning-into-data-science-here-are-your-best-resources-c31928b53cd1/undefined" rel="noopener">Edouard Harris</a>’s piece on ‘<a href="https://towardsdatascience.com/the-cold-start-problem-how-to-build-your-machine-learning-portfolio-6718b4ae83e9" rel="noopener">The cold start problem: how to build your machine learning portfolio</a>’</p><h4 id="skills-and-qualifications"><strong>Skills and qualifications</strong></h4><p>We left this section for last since it aggregates much of the information from the previous sections, but is specifically geared towards data science interview preparation. There are six main topics during a data scientist interview:</p><ol><li>Coding</li><li>Product</li><li>SQL</li><li>A/B testing</li><li>Machine Learning</li><li>Probability (see a good definition vs. Statistics <a href="https://www3.cs.stonybrook.edu/~skiena/jaialai/excerpts/node12.html" rel="noopener">here</a>)</li></ol><p>You’ll notice that one of these topics is not like the others (Product). For data science positions, <a href="https://medium.com/comet-ml/a-data-scientists-guide-to-communicating-results-c79a5ef3e9f1" rel="noopener">communication about technical concepts and results</a> as well as business metrics and impact is crucial.</p><blockquote><strong>Some useful aggregations of data science interview questions:</strong></blockquote><blockquote>?? ht<a href="https://github.com/kojino/120-Data-Science-Interview-Questions" rel="noopener">tps://github.com/kojino/120-Data-Science-Interview-Questions</a></blockquote><blockquote><a href="https://github.com/kojino/120-Data-Science-Interview-Questions" rel="noopener">??ht</a><a href="https://github.com/iamtodor/data-science-interview-questions-and-answers" rel="noopener">tps://github.com/iamtodor/data-science-interview-questions-and-answers</a></blockquote><blockquote><a href="https://github.com/iamtodor/data-science-interview-questions-and-answers" rel="noopener">???? http</a><a href="https://hookedondata.org/red-flags-in-data-science-interviews/" rel="noopener">s://hookedondata.org/red-flags-in-data-science-interviews/</a></blockquote><blockquote><a href="https://hookedondata.org/red-flags-in-data-science-interviews/" rel="noopener">?? ht</a><a href="https://medium.com/@XiaohanZeng/i-interviewed-at-five-top-companies-in-silicon-valley-in-five-days-and-luckily-got-five-job-offers-25178cf74e0f" rel="noopener">tps://medium.com/@XiaohanZeng/i-interviewed-at-five-top-companies-in-silicon-valley-in-five-days-and-luckily-got-five-job-offers-25178cf74e0f</a></blockquote><p><a href="https://medium.com/@XiaohanZeng/i-interviewed-at-five-top-companies-in-silicon-valley-in-five-days-and-luckily-got-five-job-offers-25178cf74e0f" rel="noopener">You’ll notice that we included Hooked on Data’s piece on ‘</a><a href="https://hookedondata.org/red-flags-in-data-science-interviews/" rel="noopener">Red Flags in Data Science Interviews</a>’ — as you interview for roles, you’ll come across companies who are still building up their data infrastructure or may not have a solid understanding of how their data science team fits into the larger company value.</p><p>These companies may still be climbing up this hierarchy of needs below.</p><figure><img src="https://cdn-media-1.freecodecamp.org/images/1*7IMev5xslc9FLxr9hHhpFw.png" alt=""><figcaption>The popular AI Hierarchy of Needs from Monica Rogati</figcaption></figure><p>For some expectation setting around data science interviews, I would recommend reading Tim Hopper’s piece on ‘<a href="https://tdhopper.com/blog/some-reflections-on-being-turned-down-for-a-lot-of-data-science-jobs/" rel="noopener">Some Reflections on Being Turned Down for a Lot of Data Science Jobs</a>’</p><h4 id="thanks-for-reading-we-hope-this-guide-helps-you-understand-if-data-science-is-a-career-you-should-consider-and-how-to-begin-that-journey-">Thanks for reading! We hope this guide helps you understand if data science is a career you should consider and how to begin that journey!</h4><p><em>Want to see the latest data science roles? Subscribe to the biweekly <a href="https://www.getrevue.co/profile/mljobs" rel="noopener">ML Jobs Newsletter</a> for new data science job openings in your inbox:</em></p><p><a href="https://www.getrevue.co/profile/mljobs" rel="noopener"><strong>ML Jobs Newsletter - Revue</strong></a><br><a href="https://www.getrevue.co/profile/mljobs" rel="noopener"><em>Sign up to receive this biweekly curated list of data science job openings at the best companies in the industry. Roles…</em>www.getrevue.co</a></p> </div> <hr> <p> Learn to code for free. freeCodeCamp's open source curriculum has helped more than 40,000 people get jobs as developers. <a id="learn-to-code-cta" rel="noopener noreferrer" target="_blank">Get started</a> </p> </section> </article> </div></div>
--   """
