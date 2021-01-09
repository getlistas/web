module Util where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), Replacement(..), contains, replace)
import Data.String.Regex (regex, match, test) as Regex
import Data.String.Regex.Flags (noFlags) as Regex

isUrl :: String -> Boolean
isUrl str =
  fromMaybe false
    $ hush
    $ (\rgx -> Regex.test <$> rgx <*> Right str)
    $ Regex.regex "^https?://" Regex.noFlags

takeDomain :: String -> Maybe String
takeDomain url =
  map toWebsiteName
    $ map (replace (Pattern "www.") (Replacement ""))
    $ (NEA.head =<< _)
    $ join
    $ hush
    $ (\rgx -> Regex.match <$> rgx <*> Right url)
    -- https://regexr.com/5jf24
    $ Regex.regex "[a-zA-Z-_]+(?:\\.[a-zA-Z-_]+)+" Regex.noFlags

toWebsiteName :: String -> String
toWebsiteName = case _ of
  str | contains (Pattern "twitter.com") str -> "Twitter"
  str | contains (Pattern "music.youtube.com") str -> "YouTube Music"
  str | contains (Pattern "youtube.com") str -> "YouTube"
  str | contains (Pattern "youtu.be") str -> "YouTube"
  str | contains (Pattern "vimeo.com") str -> "Vimeo"
  str | contains (Pattern "reddit.com") str -> "Reddit" -- TODO: include the subreddit
  str | contains (Pattern "spotify.com") str -> "Spotify"
  str | contains (Pattern "medium.com") str -> "Medium"
  str | contains (Pattern "dev.to") str -> "DEV Community"
  str | contains (Pattern "itunes.apple.com") str && contains (Pattern "podcast") str -> "Apple Podcasts"
  str | contains (Pattern "podcasts.apple.com") str -> "Apple Podcasts"
  str | contains (Pattern "itunes.apple.com") str && contains (Pattern "music") str -> "Apple Music"
  str | contains (Pattern "music.apple.com") str -> "Apple Music"
  str | contains (Pattern "twitch.tv") str -> "Twitch"
  str | contains (Pattern "github.com") str -> "GitHub"
  str -> str
