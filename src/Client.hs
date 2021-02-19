{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}


module Main where


import           Control.Monad.Catch         (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Time.Calendar          (Day)
import           Data.Time.Clock             (getCurrentTime, UTCTime (utctDay), addUTCTime, nominalDay)
import           Language.Javascript.JSaddle (runJSaddle)
import           Servant.API
import           Shpadoinkle                 (shpadoinkle, Html,
                                              JSM, MonadJSM, newTVarIO,
                                              MonadUnliftIO (askUnliftIO),
                                              askJSM, UnliftIO (UnliftIO))
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html
import           Shpadoinkle.Router.Client   (ClientM, client, runXHR)
import           Shpadoinkle.Run             (live, runJSorWarp)

import           Types
import           Types.Api


class TestifyEffects m where
  getAgenda :: Day -> m AgendaResult
  testify :: Submission -> m TestifyResult


newtype UIM a = UIM { runUIM :: JSM a }
  deriving newtype ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch )
#ifndef ghcjs_HOST_OS
  deriving newtype MonadJSM
#endif

instance MonadUnliftIO UIM where
  askUnliftIO = do
    c <- askJSM
    return $ UnliftIO $ \(UIM m) -> runJSaddle @IO c m

instance TestifyEffects UIM where
  getAgenda = UIM . runXHR . getAgendaM
  testify = UIM . runXHR . testifyM


getAgendaM :: Day -> ClientM AgendaResult
testifyM :: Submission -> ClientM TestifyResult
getAgendaM :<|> testifyM = client (Proxy @Api)


type Effects m =
  ( MonadThrow m
  , MonadCatch m
  , MonadJSM m
  , TestifyEffects m
  )


data ViewModel =
  ViewModel
  { vmDay       :: Day
  , vmAgenda    :: Agenda
  , vmPositions :: Positions
  , vmPersons   :: [PersonalInfo]
  }
  deriving (Eq, Show)

emptyViewModel :: Day -> ViewModel
emptyViewModel day = ViewModel day (Agenda mempty) (Positions mempty) []


view :: ViewModel -> Html m ViewModel
view _ = "hello world"


app :: JSM ()
app = do
  now <- liftIO getCurrentTime
  let tomorrow = utctDay (addUTCTime nominalDay now)
      initialModel = emptyViewModel tomorrow
  model <- newTVarIO (emptyViewModel tomorrow)
  shpadoinkle runUIM runParDiff initialModel model view getBody


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = do
  putStrLn "\nHappy point of view on https://localhost:8080\n"
  runJSorWarp 8080 app
