## TODO

- [ ] Why does navigation from Home to Login causes the nav to not render correctly?

## Notes

- Update codecs: https://github.com/thomashoneyman/purescript-halogen-realworld/commit/592eab66544b8768153ea2c415de58d7445d9267
- Skeletons
  - https://codepen.io/havardob/pen/dyGGGzq
  - [Tailwind](https://www.youtube.com/watch?v=_OZYvKsn60g)
- Google Auth: https://github.com/FrigoEU/purescript-google-auth
- `GET /user/{id}` to fetch public user information
- Can we share types with the BE? Rust <--> PureScript
- Try out `MaybeT`
- How-to images ([source](https://imgur.com/a/xtkzWhG))

[heterogeneous](https://pursuit.purescript.org/packages/purescript-heterogeneous)

---

## Questions

- What are the best approaches to work with halogen-store?
  - Should I split into smaller stores?
  - Should I create actions or keep emiting update functions instead?

## Snippets

### Work with records

```
import Prim.Row (class Lacks)
import Record as Record

addKey :: forall r a. Lacks "key" r => a -> Record r -> {key :: a| r}
addKey = Record.insert (Proxy :: Proxy "key")
```

---

## Decode with user

```purescript
listWitIdAndUserCodec :: Maybe ID -> JsonCodec ListWithIdAndUser
listWitIdAndUserCodec mbUserId = mapCodec to from codec
  where
  codec =
    CAR.object "List"
      { id: ID.codec
      , slug: slugCodec
      , title: CA.string
      , description: CAC.maybe CA.string
      , tags: CAC.array CA.string
      , user: ID.codec
      , is_public: CA.boolean
      , created_at: DateTime.codec
      , updated_at: DateTime.codec
      , fork: CAC.maybe forkMetaCodec
      }

  to :: { | ListWithIdAndUserRep ( fork :: Maybe ForkMeta ) } -> Either JsonDecodeError ListWithIdAndUser
  to {id, slug, title, description, tags, user, is_public, created_at, updated_at, fork} = pure do
    let mkList = {id, slug, title, description, tags, user, is_public, created_at, updated_at, fork, author: _ }

    mkList case mbUserId of
      Just userId | user == userId -> You
      _ -> Other id

  from :: ListWithIdAndUser -> { | ListWithIdAndUserRep ( fork :: Maybe ForkMeta ) }
  from {id, slug, title, description, tags, user, is_public, created_at, updated_at, fork} = do
    {id, slug, title, description, tags, user, is_public, created_at, updated_at, fork}

listWitIdUserAndMetaCodec :: Maybe ID -> JsonCodec ListWithIdUserAndMeta
listWitIdUserAndMetaCodec mbUserId = mapCodec to from codec
  where
  codec =
    CAR.object "List"
      { id: ID.codec
      , slug: slugCodec
      , title: CA.string
      , description: CAC.maybe CA.string
      , tags: CAC.array CA.string
      , user: ID.codec
      , is_public: CA.boolean
      , created_at: DateTime.codec
      , updated_at: DateTime.codec
      , resource_metadata: resourceMetaCodec
      , fork: CAC.maybe forkMetaCodec
      }

  to :: { | ListWithIdAndUserRep ( fork :: Maybe ForkMeta, resource_metadata :: ResourceMeta ) } -> Either JsonDecodeError ListWithIdUserAndMeta
  to {id, slug, title, description, tags, user, is_public, created_at, updated_at, fork, resource_metadata} = pure do
    let mkList = {id, slug, title, description, tags, user, is_public, created_at, updated_at, fork, resource_metadata, author: _ }

    mkList case mbUserId of
      Just userId | user == userId -> You
      _ -> Other id

  from :: ListWithIdUserAndMeta -> { | ListWithIdAndUserRep ( fork :: Maybe ForkMeta, resource_metadata :: ResourceMeta ) }
  from {id, slug, title, description, tags, user, is_public, created_at, updated_at, fork, resource_metadata} = do
    {id, slug, title, description, tags, user, is_public, created_at, updated_at, fork, resource_metadata}
```
