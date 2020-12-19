module ConfigProvider where

import Listasio.Api.Request (BaseURL(..))

provide :: BaseURL
provide = BaseURL "https://doneq.herokuapp.com"
