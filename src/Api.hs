{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}


module Api ( app ) where


import Automate
import Config
import Types
import Types.Api

import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Aeson (Value (String))
import Data.Proxy
import Data.Time.Calendar
import GHC.Generics
import Servant
import Test.WebDriver (Browser (..), sessions, runWD, useBrowser, createSession, runSession)
import Test.WebDriver.Capabilities (Capabilities (additionalCaps), defaultCaps, phantomjs)
import Test.WebDriver.Config (WDConfig (wdCapabilities), defaultConfig)
import Test.WebDriver.Session (WDSession)


app :: Config -> IO Application
app = fmap (serve api) . server


api :: Proxy Api
api = Proxy


newtype AutomateT m a = AutomateT { unAutomateT :: ReaderT Config (ExceptT ServerError m) a }

deriving instance Functor m => Functor (AutomateT m)
deriving instance Monad m => Applicative (AutomateT m)
deriving instance Monad m => Monad (AutomateT m)
deriving instance MonadIO m => MonadIO (AutomateT m)

instance Monad m => HasConfig (AutomateT m) where
  getConfig = AutomateT $ asks id


browser :: Browser
browser = Phantomjs (Just "/run/current-system/sw/bin/phantomjs") []


sessionConfig :: Config -> WDConfig
sessionConfig cfg =
  useBrowser browser
  defaultConfig
    { wdCapabilities = (wdCapabilities defaultConfig)
      { additionalCaps = [ ( "phantomjs.page.settings.userAgent"
                           ,  String . unUserAgentString $ userAgentString cfg )
                         ]
      }
    }


server :: Config -> IO (Server Api)
server cfg =
  return $ hoistServer api f server'

  where
    f :: forall x. AutomateT IO x -> Handler x
    f m = Handler $ runReaderT (unAutomateT m) cfg


server' :: HasConfig m => MonadIO m
       => ServerT Api m
server' = getAgendaHandler :<|> testifyHandler


getAgendaHandler :: HasConfig m => MonadIO m
                 => Day -> m AgendaResult
getAgendaHandler day = do
  cfg <- getConfig
  liftIO $ AgendaResult . Right <$> runSession (sessionConfig cfg) (getHouseBills cfg day)


testifyHandler :: HasConfig m => MonadIO m
               => Submission -> m TestifyResult
testifyHandler subm = do
  cfg <- getConfig
  liftIO $ TestifyResult (Right Success)
    <$ runSession (sessionConfig cfg) (testifyOnHouseBills cfg subm)
