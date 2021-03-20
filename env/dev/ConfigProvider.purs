module ConfigProvider where

import Listasio.Api.Request (BaseURL(..))

provide :: BaseURL
provide = BaseURL "http://localhost:8081"

splitbeeUrl :: String
splitbeeUrl = "https://analytics.listas.workers.dev/sb.js"
