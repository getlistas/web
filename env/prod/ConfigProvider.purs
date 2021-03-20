module ConfigProvider where

import Listasio.Api.Request (BaseURL(..))

provide :: BaseURL
provide = BaseURL "https://doneq.herokuapp.com"

splitbeeUrl :: String
splitbeeUrl = "https://analytics.listas.workers.dev/sb.js"
