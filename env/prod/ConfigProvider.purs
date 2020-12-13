module ConfigProvider where

import Doneq.Api.Request (BaseURL(..))

provide :: BaseURL
provide = BaseURL "https://doneq.herokuapp.com"
