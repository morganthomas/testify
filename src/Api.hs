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
import Network.Wai (modifyResponse, mapResponseHeaders)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant
import Test.WebDriver (Browser (..), sessions, runWD, useBrowser, createSession, runSession)
import Test.WebDriver.Capabilities (Capabilities (additionalCaps), defaultCaps, phantomjs)
import Test.WebDriver.Config (WDConfig (wdCapabilities), defaultConfig)
import Test.WebDriver.Session (WDSession)


corsMiddleware :: Application -> Application
corsMiddleware =
  modifyResponse . mapResponseHeaders $
    (("access-control-allow-origin", "*") :)
  . (("access-control-allow-methods", "GET, POST") :)
  . (("access-control-allow-headers", "*") :)


app :: Config -> IO Application
app = fmap (corsMiddleware . provideOptions api . serve api) . server


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
browser = Phantomjs (Just "/nix/store/afygm6wrkdr2g7g8079kjd3lhz1vlnmx-phantomjs-2.1.1/bin/phantomjs") []


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
