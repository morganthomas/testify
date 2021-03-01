{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}


module Main where


import           Prelude                     hiding (div)

import           Control.Concurrent.STM.TVar.Lifted (modifyTVarIO)
import           Control.Lens                ((.~))
import           Control.Monad.Catch         (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Data.Generics.Labels        ()
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Text                   (Text, pack)
import           Data.Time.Calendar          (Day, toGregorian, fromGregorian)
import           Data.Time.Clock             (getCurrentTime, UTCTime (utctDay), addUTCTime, nominalDay)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (runJSaddle)
import           Servant.API
import           Shpadoinkle                 (shpadoinkle, Html, NFData,
                                              forgetC, liftC, voidRunContinuationT,
                                              pur, impur, commit, merge,
                                              JSM, MonadJSM, newTVarIO,
                                              MonadUnliftIO (askUnliftIO),
                                              askJSM, UnliftIO (UnliftIO))
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.Html
import           Shpadoinkle.Lens            (onRecord)
import           Shpadoinkle.Router.Client   (ClientM, client, runXHR)
import           Shpadoinkle.Run             (live, runJSorWarp)
import           UnliftIO.Concurrent         (forkIO)

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


data IsLoadingAgenda = IsLoadingAgenda | IsNOTLoadingAgenda
  deriving (Eq, Generic, Show)

instance NFData IsLoadingAgenda


data SubmissionStatus = HaveNotSubmitted | SubmissionProcessing | SubmissionSucceeded | SubmissionFailed ErrorMessage
  deriving (Eq, Generic, Show)

instance NFData SubmissionStatus


data ViewModel =
  ViewModel
  { vmDay       :: Day
  , vmIsLoading :: IsLoadingAgenda
  , vmAgenda    :: Maybe AgendaResult
  , vmPositions :: Positions
  , vmPersons   :: [PersonalInfo]
  , vmStatus    :: SubmissionStatus
  }
  deriving (Eq, Generic, Show)

instance NFData ViewModel

emptyViewModel :: Day -> ViewModel
emptyViewModel day = ViewModel day IsNOTLoadingAgenda Nothing (Positions mempty) [] HaveNotSubmitted


newtype Year = Year { unYear :: Integer }
  deriving (Eq, Generic)
  deriving newtype Show


newtype MonthOfYear = MonthOfYear { unMonthOfYear :: Int }
  deriving (Eq, Generic)

instance Show MonthOfYear where
  show (MonthOfYear n) =
    case n of
      1  -> "January"
      2  -> "February"
      3  -> "March"
      4  -> "April"
      5  -> "May"
      6  -> "June"
      7  -> "July"
      8  -> "August"
      9  -> "September"
      10 -> "October"
      11 -> "November"
      12 -> "December"
      _  -> "Unknown Month"


newtype DayOfMonth = DayOfMonth { unDayOfMonth :: Int }
  deriving (Eq, Generic, Show)


getYear :: Day -> Year
getYear day = let (y, _, _) = toGregorian day in Year y


getMonth :: Day -> MonthOfYear
getMonth day = let (_, m, _) = toGregorian day in MonthOfYear m


getDay :: Day -> DayOfMonth
getDay day = let (_, _, d) = toGregorian day in DayOfMonth d


setYear :: Year -> Day -> Day
setYear (Year y) day = let (_, m, d) = toGregorian day in fromGregorian y m d


setMonth :: MonthOfYear -> Day -> Day
setMonth (MonthOfYear m) day = let (y, _, d) = toGregorian day in fromGregorian y m d


setDay :: DayOfMonth -> Day -> Day
setDay (DayOfMonth d) day = let (y, m, _) = toGregorian day in fromGregorian y m d


selectFrom :: Applicative m => Eq a => Show a => [a] -> a -> Html m a
selectFrom opts oSelected =
  select []
  ( (\o ->
      option
        [ value (pack (show o))
        , onClick (const o)
        , selected (o == oSelected)
        ]
        [ text (pack (show o)) ]
    )
    <$> opts )


years :: [Year]
years = Year <$> [2020..2050]


yearSelect :: Applicative m => Year -> Html m Year
yearSelect = selectFrom years


months :: [MonthOfYear]
months = MonthOfYear <$> [1..12]


monthSelect :: Applicative m => MonthOfYear -> Html m MonthOfYear
monthSelect = selectFrom months


days :: [DayOfMonth]
days = DayOfMonth <$> [1..31]


dayOfMonthSelect :: Applicative m => DayOfMonth -> Html m DayOfMonth
dayOfMonthSelect = selectFrom days


dateSelect :: Applicative m => Day -> Html m Day
dateSelect day =
  let (y, m, d) = toGregorian day in
  div
    [ class' "date-select" ]
    [ liftC setYear  getYear  $ yearSelect (Year y)
    , liftC setMonth getMonth $ monthSelect (MonthOfYear m)
    , liftC setDay   getDay   $ dayOfMonthSelect (DayOfMonth m)
    ]


getAgendaButton :: Monad m => TestifyEffects m => Day -> Html m ViewModel
getAgendaButton day =
  button
    [ onClickC . voidRunContinuationT $ do
        commit . pur $ \m -> m { vmIsLoading = IsLoadingAgenda }
        commit . merge . impur $ (\a m -> m { vmAgenda = a }) . Just <$> getAgenda day
        commit . pur $ \m -> m { vmIsLoading = IsNOTLoadingAgenda }
    ]
    [ text "Get Agenda" ]


agendaView :: Applicative m => ViewModel -> Html m ViewModel
agendaView _ = div [] []


personsView :: Applicative m => [PersonalInfo] -> Html m [PersonalInfo]
personsView _ = div [] []


submitButton :: Monad m => TestifyEffects m
             => ViewModel -> Html m ViewModel
submitButton vm =
  case vmAgenda vm of
    Just (AgendaResult (Right _)) ->
      if null (unPositions (vmPositions vm))
      then disabledButton
      else enabledButton
    _ -> disabledButton
  where
    disabledButton =
      button
        [ disabled True ]
        [ text "Submit" ]

    enabledButton =
      button
        [ onClickC . voidRunContinuationT $ do
            commit . pur $ \m -> m { vmStatus = SubmissionProcessing }
            commit . merge . impur $ (\res m -> m { vmStatus = toStatus res }) <$> testify submission
        ]
        [ text "Submit" ]

    toStatus (TestifyResult (Left err)) = SubmissionFailed err
    toStatus (TestifyResult (Right Success)) = SubmissionSucceeded

    submission = Submission (vmPositions vm) (vmPersons vm) (vmDay vm)


statusView :: Applicative m => SubmissionStatus -> Html m ()
statusView _ = div [] []


view :: Effects m => ViewModel -> Html m ViewModel
view model =
  div
    [class' "app"]
    [ onRecord #vmDay $ dateSelect (vmDay model)
    , getAgendaButton (vmDay model)
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
