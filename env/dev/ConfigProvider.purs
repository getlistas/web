module ConfigProvider where

import Doneq.Api.Request (BaseURL(..))

provide :: BaseURL
provide = BaseURL "http://localhost:8080"
