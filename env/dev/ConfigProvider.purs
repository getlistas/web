module ConfigProvider where

import Listasio.Api.Request (BaseURL(..))

provide :: BaseURL
provide = BaseURL "http://localhost:8080"
