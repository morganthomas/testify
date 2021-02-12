{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}


module Types.Api where


import Servant.API

import Types


type Api = AgendaApi :<|> TestifyApi


type AgendaApi = "agenda" :> Get '[JSON] AgendaResult


type TestifyApi = "testify" :> ReqBody '[JSON] Submission :> Post '[JSON] TestifyResult
