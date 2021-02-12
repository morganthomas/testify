module Api ( api ) where


import Automate
import Config
import Types
import Types.Api

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Proxy
import Data.Time.Calendar
import GHC.Generics
import Servant
import Test.WebDriver.Session (WDSession)


app :: MonadIO m => HasConfig m => HasWebDriver m
    => m Application
app = error "todo"


api :: Proxy Api
api = Proxy


newtype AutomateT m a = AutomateT { unAutomateT :: ReaderT (Config, WDSession) (ExceptT ServerError m) a }


server :: Config -> IO (Server Api)
server = error "todo"


server' :: Config -> ServerT Api (AutomateT IO)
server' = error "todo"


server'' :: HasConfig m => HasWebDriver m => MonadIO m
       => ServerT Api m
server'' = getAgendaHandler :<|> testifyHandler


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
