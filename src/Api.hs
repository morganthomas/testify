module Api ( api ) where


import Automate
import Config
import Types
import Types.Api

import Control.Monad.IO.Class
import Data.Time.Calendar
import Servant


api :: HasConfig m => HasWebDriver m => MonadIO m
    => ServerT Api m
api = getAgendaHandler :<|> testifyHandler


getAgendaHandler :: HasConfig m => HasWebDriver m => MonadIO m
                 => Day -> m AgendaResult
getAgendaHandler day = do
  cfg <- getConfig
  AgendaResult . Right <$> runWebDriver (getHouseBills cfg day)


testifyHandler :: HasConfig m => HasWebDriver m => MonadIO m
               => Submission -> m TestifyResult
testifyHandler subm = do
  cfg <- getConfig
  TestifyResult (Right Success)
    <$ runWebDriver (testifyOnHouseBills cfg subm)
