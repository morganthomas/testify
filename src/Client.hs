{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}


module Main where


import           Prelude                     hiding (div)

import           Control.Lens                ()
import           Control.Monad.Catch         (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Data.Generics.Labels        ()
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Text                   (Text)
import           Data.Time.Calendar          (Day)
import           Data.Time.Clock             (getCurrentTime, UTCTime (utctDay), addUTCTime, nominalDay)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (runJSaddle)
import           Servant.API
import           Shpadoinkle                 (shpadoinkle, Html,
                                              forgetC,
                                              JSM, MonadJSM, newTVarIO,
                                              MonadUnliftIO (askUnliftIO),
                                              askJSM, UnliftIO (UnliftIO))
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html
import           Shpadoinkle.Lens            (onRecord)
import           Shpadoinkle.Router.Client   (ClientM, client, runXHR)
import           Shpadoinkle.Run             (live, runJSorWarp)

import           Types
import           Types.Api

default (Text)


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

instance ( Monad m, MonadTrans t, TestifyEffects m ) => TestifyEffects (t m) where
  getAgenda = lift . getAgenda
  testify = lift . testify


getAgendaM :: Day -> ClientM AgendaResult
testifyM :: Submission -> ClientM TestifyResult
getAgendaM :<|> testifyM = client (Proxy @Api)


type Effects m =
  ( MonadThrow m
  , MonadCatch m
  , MonadJSM m
  , TestifyEffects m
  )


data IsLoadingAgenda = IsLoadingAgenda | LoadedAgenda
  deriving (Eq, Generic, Show)


data SubmissionStatus = HaveNotSubmitted | SubmissionProcessing | SubmissionSucceeded | SubmissionFailed ErrorMessage
  deriving (Eq, Generic, Show)


data ViewModel =
  ViewModel
  { vmDay       :: Day
  , vmIsLoading :: IsLoadingAgenda
  , vmAgenda    :: Agenda
  , vmPositions :: Positions
  , vmPersons   :: [PersonalInfo]
  , vmStatus    :: SubmissionStatus
  }
  deriving (Eq, Generic, Show)

emptyViewModel :: Day -> ViewModel
emptyViewModel day = ViewModel day IsLoadingAgenda (Agenda mempty) (Positions mempty) [] HaveNotSubmitted


daySelect :: Day -> Html m Day
daySelect _ = div [] []


agendaView :: ViewModel -> Html m ViewModel
agendaView _ = div [] []


personsView :: [PersonalInfo] -> Html m [PersonalInfo]
personsView _ = div [] []


submitButton :: ViewModel -> Html m ViewModel
submitButton _ = div [] []


statusView :: SubmissionStatus -> Html m ()
statusView _ = div [] []


view :: Effects m => ViewModel -> Html m ViewModel
view model =
  div
    [class' "app"]
    [ onRecord #vmDay $ daySelect (vmDay model)
    , agendaView model
    , onRecord #vmPersons $ personsView (vmPersons model)
    , submitButton model
    , forgetC $ statusView (vmStatus model)
    ]


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
