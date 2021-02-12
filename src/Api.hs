module Api ( api ) where


import Types
import Types.Api

import Control.Monad.IO.Class
import Servant


api :: MonadIO m => ServerT Api m
api = getAgendaHandler :<|> testifyHandler


getAgendaHandler :: MonadIO m => m AgendaResult
getAgendaHandler = error "todo"


testifyHandler :: MonadIO m => Submission -> m TestifyResult
testifyHandler = error "todo"
