{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}


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
import Test.WebDriver (runWD)
import Test.WebDriver.Session (WDSession)


createWebDriver :: MonadIO m => m WDSession
createWebDriver = error "todo"


app :: Config -> IO Application
app = fmap (serve api) . server


api :: Proxy Api
api = Proxy


newtype AutomateT m a = AutomateT { unAutomateT :: ReaderT (Config, WDSession) (ExceptT ServerError m) a }

deriving instance Functor m => Functor (AutomateT m)
deriving instance Monad m => Applicative (AutomateT m)
deriving instance Monad m => Monad (AutomateT m)
deriving instance MonadIO m => MonadIO (AutomateT m)

instance Monad m => HasConfig (AutomateT m) where
  getConfig = AutomateT $ asks fst

instance MonadIO m => HasWebDriver (AutomateT m) where
  runWebDriver m = do
    session <- AutomateT $ asks snd
    liftIO $ runWD session m


server :: Config -> IO (Server Api)
server cfg = do
  session <- createWebDriver
  return $ hoistServer api (f session) server'

  where
    f :: WDSession -> forall x. AutomateT IO x -> Handler x
    f session m = Handler $ runReaderT (unAutomateT m) (cfg, session)


server' :: HasConfig m => HasWebDriver m => MonadIO m
       => ServerT Api m
server' = getAgendaHandler :<|> testifyHandler


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
