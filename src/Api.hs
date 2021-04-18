{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module Api ( app ) where


import Automate
import Client
import Config
import Types
import Types.Api

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Aeson (Value (String))
import Data.Proxy
import Data.Time.Calendar
import GHC.Generics
import Language.Javascript.JSaddle (MonadJSM (..))
import Network.HTTP.Types (Method)
import Network.Wai (modifyResponse, mapResponseHeaders)
import Network.Wai.Application.Static (defaultWebAppSettings)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant
import Servant.Foreign.Internal (GenerateList (..), HasForeign (..), Req)
import Shpadoinkle.Router (View)
import Shpadoinkle.Router.Server (serveDirectoryWithSpa)
import Test.WebDriver (Browser (..), sessions, runWD, useBrowser, createSession, runSession)
import Test.WebDriver.Capabilities (Capabilities (additionalCaps), defaultCaps, phantomjs)
import Test.WebDriver.Config (WDConfig (wdCapabilities), defaultConfig)
import Test.WebDriver.Session (WDSession)


data Noop a = Noop
  deriving anyclass ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, TestifyEffects )


#ifndef ghcjs_HOST_OS
instance MonadJSM Noop where
  liftJSM' = const Noop
#endif


type SPA = View Noop ViewModel


type App = Api :<|> SPA


corsMiddleware :: Application -> Application
corsMiddleware =
  modifyResponse . mapResponseHeaders $
    (("access-control-allow-origin", "*") :)
  . (("access-control-allow-methods", "GET, POST") :)
  . (("access-control-allow-headers", "*") :)


-- This and the subsequent instance are just crap to get this to compile
instance HasForeign lang ftype (View effect vm) where
  type Foreign ftype (View effect vm) = Foreign ftype Raw
  foreignFor l f a req = foreignFor l f (Proxy :: Proxy Raw) req

instance GenerateList NoContent (Method -> Req NoContent) where
  generateList _ = []



app :: Config -> IO Application
#ifndef ghcjs_HOST_OS
app = fmap (corsMiddleware . provideOptions appPxy . serve appPxy) . server
#else
app = fmap (serve appPxy) . server
#endif


appPxy :: Proxy App
appPxy = Proxy


newtype AutomateT m a = AutomateT { unAutomateT :: ReaderT Config (ExceptT ServerError m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance Monad m => HasConfig (AutomateT m) where
  getConfig = AutomateT $ asks id


browser :: PhantomjsPath -> Browser
browser phantomPath = Phantomjs (Just (unPhantomjsPath phantomPath)) []


sessionConfig :: Config -> WDConfig
sessionConfig cfg =
  useBrowser (browser (phantomjsPath cfg))
  defaultConfig
    { wdCapabilities = (wdCapabilities defaultConfig)
      { additionalCaps = [ ( "phantomjs.page.settings.userAgent"
                           ,  String . unUserAgentString $ userAgentString cfg )
                         ]
      }
    }


server :: Config -> IO (Server App)
server cfg =
  return $ hoistServer appPxy f server'

  where
    f :: forall x. AutomateT IO x -> Handler x
    f m = Handler $ runReaderT (unAutomateT m) cfg


server' :: HasConfig m => MonadIO m
       => ServerT App m
server' = (getAgendaHandler :<|> testifyHandler) :<|> spaHandler


getAgendaHandler :: HasConfig m => MonadIO m
                 => Day -> Chamber -> m AgendaResult
getAgendaHandler day chamber = do
  cfg <- getConfig
  liftIO $ AgendaResult . Right <$> runSession (sessionConfig cfg) (getBills cfg day chamber)


testifyHandler :: HasConfig m => MonadIO m
               => Submission -> m TestifyResult
testifyHandler subm = do
  cfg <- getConfig
  liftIO $ TestifyResult (Right Success)
    <$ runSession (sessionConfig cfg) (testifyOnBills cfg subm)


spaHandler :: MonadIO m => ServerT SPA m
spaHandler = serveDirectoryWithSpa (defaultWebAppSettings "./static")
