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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}


module Main where


import           Prelude                     hiding (div, span)

import           Control.Concurrent.STM.TVar.Lifted (modifyTVarIO)
import           Control.Lens                (Lens', (.~), (^.), at)
import           Control.Lens.Prism          (_Just)
import           Control.Monad.Catch         (MonadThrow, MonadCatch)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Data.Generics.Labels        ()
import qualified Data.Map                    as Map
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
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
import           Shpadoinkle.DeveloperTools  (withDeveloperTools)
import           Shpadoinkle.Html
import           Shpadoinkle.Lens            (onRecord)
import           Shpadoinkle.Router.Client   (ClientEnv (ClientEnv), BaseUrl (BaseUrl), Scheme (Http), ClientM, client, runXHR')
import           Shpadoinkle.Run             (live, runJSorWarp)
import           UnliftIO.Concurrent         (forkIO)

import           Types
import           Types.Api

default (Text)


features :: Features
features = Features NoMultiPersonFeature


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

clientEnv :: ClientEnv
clientEnv = ClientEnv (BaseUrl Http "localhost" 8008 "")

toUIM :: ClientM a -> UIM a
toUIM = UIM . flip runXHR' clientEnv

instance TestifyEffects UIM where
  getAgenda = toUIM . getAgendaM
  testify = toUIM . testifyM

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
  deriving (Eq, Generic, Read, Show)

instance NFData IsLoadingAgenda


data SubmissionStatus = HaveNotSubmitted | SubmissionProcessing | SubmissionSucceeded | SubmissionFailed ErrorMessage
  deriving (Eq, Generic, Read, Show)

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
  deriving (Eq, Generic, Read, Show)

instance NFData ViewModel

emptyViewModel :: Day -> ViewModel
emptyViewModel day =
  ViewModel day IsNOTLoadingAgenda Nothing (Positions mempty) initialPersons HaveNotSubmitted
  where
    initialPersons =
      case multiPersonFeature features of
        MultiPersonFeature -> []
        NoMultiPersonFeature -> [emptyPersonalInfo]


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
  deriving (Eq, Generic)
  deriving newtype Show


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
    <$> opts
  )


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
    , liftC setDay   getDay   $ dayOfMonthSelect (DayOfMonth d)
    ]


getAgendaButton :: Monad m => TestifyEffects m => Day -> Html m ViewModel
getAgendaButton day =
  button
    [ onClickC . voidRunContinuationT $ do
        commit . pur $ #vmIsLoading .~ IsLoadingAgenda
        commit . merge . impur $ (#vmAgenda .~) . Just <$> getAgenda day
        commit . pur $ #vmIsLoading .~ IsNOTLoadingAgenda
    ]
    [ text "Get Agenda" ]


agendaView :: Applicative m => ViewModel -> Html m ViewModel
agendaView vm =
  div [] $
    case vmAgenda vm of
      Just (AgendaResult (Right agenda@(Agenda agendaMap))) ->
        uncurry (committeeView vm agenda) <$> Map.toList agendaMap
      Just (AgendaResult (Left err)) ->
        [ text ("An error has occurred loading the agenda: " <> unErrorMessage err) ]
      Nothing ->
        case vmIsLoading vm of
          IsLoadingAgenda -> [ text "Loading agenda..." ]
          IsNOTLoadingAgenda -> [ ]


committeeView :: Applicative m => ViewModel -> Agenda -> Committee -> Set Bill -> Html m ViewModel
committeeView vm agenda cm bills = div [] $
    div
      [ class' "committee-header" ]
      [ text (unCommitteeName (committeeName cm)) ]
  : ( billView vm agenda cm <$> Set.toList bills )


billView :: Applicative m => ViewModel -> Agenda -> Committee -> Bill -> Html m ViewModel
billView vm agenda cm bill =
  div
    [ class' "bill" ]
    [ div
        [ class' "bill-title" ]
        [ text (unBillName (billName bill)) ]
    , div
        [ class' "bill-positions" ]
        $ billPositionView vm agenda cm bill <$> [ Support, Neutral, Oppose ]
    ]


billPositionView :: Applicative m => ViewModel -> Agenda -> Committee -> Bill -> Position -> Html m ViewModel
billPositionView _vm _agenda cm bill pos =
  span
    [ class' "bill-position" ]
    [ text (pack (show pos))
    , input
        [ ("type", "radio")
        , name' (unBillId (billId bill))
        , onClick $ #vmPositions . #unPositions . at cm . _Just . at bill . _Just .~ pos
        ]
        []
    ]


emptyPersonalInfo :: PersonalInfo
emptyPersonalInfo = PersonalInfo "" "" "" ""


editField :: Applicative m => Text -> (a -> Text) -> (Text -> a) -> Lens' PersonalInfo a -> PersonalInfo -> Html m [PersonalInfo]
editField label toTxt fromTxt lens person =
  div
    [ class' "edit-field" ]
    [ text label
    , input
        [ ("type", "text")
        , value (toTxt $ person ^. lens)
        , onInput (\new -> replace person (lens .~ fromTxt new $ person))
        ]
        []
    ]
  where
    replace :: Eq a => a -> a -> [a] -> [a]
    replace x y = fmap (\z -> if z == x then y else z)


editPerson :: Applicative m => PersonalInfo -> Html m [PersonalInfo]
editPerson person =
  div
    [ class' "edit-person" ]
    $
    [ editField "First Name" unFirstName FirstName #firstName person
    , editField "Last Name" unLastName LastName #lastName person
    , editField "Email" unEmail Email #email person
    , editField "Town" unTown Town #town person
    ]
    ++
    (
      case multiPersonFeature features of
        MultiPersonFeature ->
          [ button
              [ onClick (filter (/= person)) ]
              [ text "x" ]
          ] 
        NoMultiPersonFeature -> []
    )


addPerson :: Applicative m => [PersonalInfo] -> Html m [PersonalInfo]
addPerson persons =
  button
    [ onClick (<> [emptyPersonalInfo]) ]
    [ text "Add Person" ]


personsView :: Applicative m => [PersonalInfo] -> Html m [PersonalInfo]
personsView persons =
  case multiPersonFeature features of
    MultiPersonFeature ->
      div
        []
        (  ( editPerson <$> persons )
        <> [ addPerson persons ]
        )
    NoMultiPersonFeature ->
      div [] ( editPerson <$> persons )


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
            commit . pur $ #vmStatus .~ SubmissionProcessing
            commit . merge . impur $ (#vmStatus .~) . toStatus <$> testify submission
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
  model <- newTVarIO initialModel
  withDeveloperTools model
  shpadoinkle runUIM runParDiff model view getBody


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = do
  putStrLn "\nHappy point of view on https://localhost:8080\n"
  runJSorWarp 8080 app
