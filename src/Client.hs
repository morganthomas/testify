{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}


module Main where


import           Control.Monad.Catch         (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class      (MonadIO)
import           Data.Proxy
import           Data.Time.Calendar
import           Servant.API
import           Shpadoinkle                 (Html, JSM, MonadJSM)
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html
import           Shpadoinkle.Router.Client
import           Shpadoinkle.Run             (live, runJSorWarp, simple)

import           Types
import           Types.Api


getAgendaM :: Day -> ClientM AgendaResult
testifyM :: Submission -> ClientM TestifyResult

getAgendaM :<|> testifyM = client (Proxy @Api)


newtype UIM a = UIM { runUIM :: JSM a }
  deriving newtype ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch )
#ifndef ghcjs_HOST_OS
  deriving newtype MonadJSM
#endif


getAgenda :: Day -> UIM AgendaResult
getAgenda = UIM . runXHR . getAgendaM


testify :: Submission -> UIM TestifyResult
testify = UIM . runXHR . testifyM


view :: () -> Html m ()
view _ = "hello world"


app :: JSM ()
app = simple runParDiff () view getBody


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = do
  putStrLn "\nHappy point of view on https://localhost:8080\n"
  runJSorWarp 8080 app
