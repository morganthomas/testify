{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}


module Types.Api where


import Data.Time.Calendar
import Servant.API

import Types


type Api = AgendaApi :<|> TestifyApi


type AgendaApi = "agenda" :> Capture "day" Day :> Get '[JSON] AgendaResult


type TestifyApi = "testify" :> ReqBody '[JSON] Submission :> Post '[JSON] TestifyResult
