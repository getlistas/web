module ConfigProvider where

import Listasio.Api.Request (BaseURL(..))
import Listasio.Store (LogLevel(..))

provide :: BaseURL
provide = BaseURL "https://api.listas.io"

splitbeeUrl :: String
splitbeeUrl = "https://analytics.listas.workers.dev/sb.js"

env :: LogLevel
env = Prod
