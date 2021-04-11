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
{-# LANGUAGE TupleSections              #-}
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
import           Data.Maybe                  (fromMaybe)
import           Data.Proxy                  (Proxy (Proxy))
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text, pack)
import           Data.Time.Calendar          (Day, toGregorian, fromGregorian)
import           Data.Time.Clock             (getCurrentTime, UTCTime (utctDay), addUTCTime, nominalDay)
import           Data.Time.LocalTime         (utcToLocalTime, getCurrentTimeZone, localDay)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (runJSaddle, fromJSVal, (!))
import           Servant.API
import           Shpadoinkle                 (shpadoinkle, Html, NFData,
                                              forgetC, liftC, voidRunContinuationT,
                                              pur, impur, commit, mapC, before, causes,
                                              kleisli, merge,
                                              JSM, MonadJSM, newTVarIO,
                                              MonadUnliftIO (askUnliftIO),
                                              askJSM, UnliftIO (UnliftIO),
                                              RawNode (..))
import           Shpadoinkle.Backend.ParDiff (runParDiff)
import           Shpadoinkle.DeveloperTools  (withDeveloperTools)
import           Shpadoinkle.Html
import           Shpadoinkle.Html.LocalStorage
import           Shpadoinkle.Html.Utils      (addStyle)
import           Shpadoinkle.Lens            (onRecord)
import           Shpadoinkle.Router.Client   (ClientEnv (ClientEnv), BaseUrl (BaseUrl), Scheme (Http), ClientM, client, runXHR')
import           Shpadoinkle.Run             (live, runJSorWarp)
import           UnliftIO.Concurrent         (forkIO)

import           Config
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

initialViewModel :: Day -> [PersonalInfo] -> ViewModel
initialViewModel day persons =
  ViewModel day IsNOTLoadingAgenda Nothing (Positions mempty) persons HaveNotSubmitted


newtype Year = Year { unYear :: Integer }
  deriving (Eq, Generic)
  deriving newtype (Read, Show)


data MonthOfYear
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Eq, Generic, Read, Show)

monthToNum :: MonthOfYear -> Int
monthToNum January   = 1
monthToNum February  = 2
monthToNum March     = 3
monthToNum April     = 4
monthToNum May       = 5
monthToNum June      = 6
monthToNum July      = 7
monthToNum August    = 8
monthToNum September = 9
monthToNum October   = 10
monthToNum November  = 11
monthToNum December  = 12

numToMonth :: Int -> MonthOfYear
numToMonth 1  = January
numToMonth 2  = February
numToMonth 3  = March
numToMonth 4  = April
numToMonth 5  = May
numToMonth 6  = June
numToMonth 7  = July
numToMonth 8  = August
numToMonth 9  = September
numToMonth 10 = October
numToMonth 11 = November
numToMonth 12 = December
numToMonth _  = error "numToMonth unhandled case"


newtype DayOfMonth = DayOfMonth { unDayOfMonth :: Int }
  deriving (Eq, Generic)
  deriving newtype (Read, Show)


getYear :: Day -> Year
getYear day = let (y, _, _) = toGregorian day in Year y


getMonth :: Day -> MonthOfYear
getMonth day = let (_, m, _) = toGregorian day in numToMonth m


getDay :: Day -> DayOfMonth
getDay day = let (_, _, d) = toGregorian day in DayOfMonth d


setYear :: Year -> Day -> Day
setYear (Year y) day = let (_, m, d) = toGregorian day in fromGregorian y m d


setMonth :: MonthOfYear -> Day -> Day
setMonth m day = let (y, _, d) = toGregorian day in fromGregorian y (monthToNum m) d


setDay :: DayOfMonth -> Day -> Day
setDay (DayOfMonth d) day = let (y, m, _) = toGregorian day in fromGregorian y m d


selectFrom :: Applicative m => Eq a => Read a => Show a
           => [a] -> a -> Html m a
selectFrom opts oSelected =
  select
    [ listenRaw "change" $ \(RawNode n) _ ->
        pur . const . read . fromMaybe "" <$> (fromJSVal =<< n ! "value")
    , class' "mr-1 p-1"
    ]
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
months = numToMonth <$> [1..12]


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
    [ class' "m-2 mt-5" ]
    [ span
        [ class' "m-1 font-semibold" ]
        [ text "Select Date" ]
    , div
      [ class' "m-1" ]
      [ liftC setYear  getYear  $ yearSelect (Year y)
      , liftC setMonth getMonth $ monthSelect (numToMonth m)
      , liftC setDay   getDay   $ dayOfMonthSelect (DayOfMonth d)
      ]
    ]


btnCls :: Text
btnCls = "border-solid border-black border-2 rounded p-2 m-2 font-semibold"


getAgendaButton :: Monad m => TestifyEffects m => Day -> Html m ViewModel
getAgendaButton day =
  button
    [ onClickC . voidRunContinuationT $ do
        commit . pur $ #vmIsLoading .~ IsLoadingAgenda
        commit . pur $ #vmAgenda .~ Nothing
        commit . merge . impur $ (#vmAgenda .~) . Just <$> getAgenda day
        commit . pur $
          \vm ->
            case vmAgenda vm of
              Just (AgendaResult (Right (Agenda agenda))) ->
                (#vmPositions . #unPositions) .~
                  (Map.fromList . fmap (,Nothing) . Set.toList
                    <$> agenda)
                $ vm
              _ -> vm
        commit . pur $ #vmIsLoading .~ IsNOTLoadingAgenda
    , class' btnCls
    ]
    [ text "Get Agenda" ]


agendaView :: Applicative m => ViewModel -> Html m ViewModel
agendaView vm =
  div [ class' "m-2" ] $
    case vmAgenda vm of
      Just (AgendaResult (Right agenda@(Agenda agendaMap))) ->
        if null agendaMap
        then [ text "No bills (yay!)" ]
        else uncurry (committeeView vm agenda) <$> Map.toList agendaMap
      Just (AgendaResult (Left err)) ->
        [ text ("An error has occurred loading the agenda: " <> unErrorMessage err) ]
      Nothing ->
        case vmIsLoading vm of
          IsLoadingAgenda -> [ text "Loading agenda..." ]
          IsNOTLoadingAgenda -> [ ]


committeeView :: Applicative m => ViewModel -> Agenda -> Committee -> Set Bill -> Html m ViewModel
committeeView vm agenda cm bills = div [] $
    div
      [ class' "mb-2 font-semibold" ]
      [ text (unCommitteeName (committeeName cm)) ]
  : ( if null bills
      then [ text "No bills for this committee (is this a software error?)" ]
      else billView vm agenda cm <$> Set.toList bills )


billView :: Applicative m => ViewModel -> Agenda -> Committee -> Bill -> Html m ViewModel
billView vm agenda cm bill =
  div
    [ class' "mb-2" ]
    [ div [ ]
        [ text (unBillName (billName bill)) ]
    , div [ ]
        $ billPositionView vm agenda cm bill <$> [ Support, Neutral, Oppose ]
    ]


billPositionView :: Applicative m => ViewModel -> Agenda -> Committee -> Bill -> Position -> Html m ViewModel
billPositionView _vm _agenda cm bill pos =
  span
    [ class' "mr-2" ]
    [ text (pack (show pos))
    , input
        [ class' "mx-2"
        , ("type", "radio")
        , name' (unBillId (billId bill))
        , onClick $ #vmPositions . #unPositions . at cm . _Just . at bill . _Just .~ Just pos
        ]
        []
    ]


emptyPersonalInfo :: PersonalInfo
emptyPersonalInfo = PersonalInfo "" "" "" ""


editField :: Applicative m => Text -> (a -> Text) -> (Text -> a) -> Lens' PersonalInfo a -> PersonalInfo -> Html m [PersonalInfo]
editField label toTxt fromTxt lens person =
  div []
  [ span [ class' "w-32 inline-block" ] [ text label ]
  , span
      [ class' "w-44 inline-block" ]
      [ input
        [ ("type", "text")
        , ("size", "44")
        , value (toTxt $ person ^. lens)
        , onInput (\new -> replace person (lens .~ fromTxt new $ person))
        ]
        []
      ]
  ]
  where
    replace :: Eq a => a -> a -> [a] -> [a]
    replace x y = fmap (\z -> if z == x then y else z)


editPerson :: Applicative m => PersonalInfo -> Html m [PersonalInfo]
editPerson person =
  div [ ]
    $
    [ editField "First Name" unFirstName FirstName #firstName person
    , editField "Last Name" unLastName LastName #lastName person
    , editField "Email" unEmail Email #email person
    , editField "Town" unTown Town #town person
    ]
    <>
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


personsView :: MonadJSM m => [PersonalInfo] -> Html m [PersonalInfo]
personsView persons =
  mapC (`before` (kleisli $ \persons -> return . causes $ setSavedPersonalInfo persons))
  $
  case multiPersonFeature features of
    MultiPersonFeature ->
      div
        [ class' "m-2" ]
        (  ( editPerson <$> persons )
        <> [ addPerson persons ]
        )
    NoMultiPersonFeature ->
      div
        [ class' "m-2" ]
        $
          span [ class' "font-semibold mb-2" ] [ text "Personal Information" ]
        : ( editPerson <$> persons )


submitButton :: Monad m => TestifyEffects m
             => ViewModel -> Html m ViewModel
submitButton vm =
  case vmAgenda vm of
    Just (AgendaResult (Right _)) ->
      if null (unPositions (vmPositions vm))
      then disabledButton
      else if vmStatus vm == HaveNotSubmitted
           then enabledButton
           else disabledButton
    _ -> disabledButton
  where
    disabledButton =
      button
        [ disabled True
        , class' btnCls ]
        [ text $ case vmStatus vm of
                   SubmissionProcessing -> "Processing..."
                   SubmissionSucceeded -> "Success!"
                   SubmissionFailed err -> "Failure: " <> unErrorMessage err
                   _ -> "Submit"
        ]

    enabledButton =
      button
        [ class' btnCls
        , onClickC . voidRunContinuationT $ do
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
  div [ ]
    [ div
        [ class' "m-3 p-2 font-bold text-lg border-b-2 border-black" ]
        [ text "New Hampshire Bill Sign In Tool" ]
    , div
        [ class' "m-3" ]
        [ text "This tool allows you to record your positions on bills for the current New Hampshire legislative session." ]
    , div
        [ class' "m-3" ]
        [ text "It is a more streamlined way of using the "
        , a [ class' hrefCls, href (unHouseFormUrl (houseFormUrl cfg)) ] [ text "House sign in form" ]
        , text " and the "
        , a [ class' hrefCls, href (unSenateFormUrl (senateFormUrl cfg)) ] [ text "Senate sign in form" ]
        , text " provided by the State of New Hampshire."
        ]
    , onRecord #vmDay $ dateSelect (vmDay model)
    , getAgendaButton (vmDay model)
    , agendaView model
    , onRecord #vmPersons $ personsView (vmPersons model)
    , submitButton model
    , forgetC $ statusView (vmStatus model)
    , div
        [ class' "m-2" ]
        [ text "Please report any issues with this tool to "
        , a [ href "mailto:morgan.thomas@platonic.systems"
            , class' hrefCls ]
            [ "Morgan Thomas <morgan.thomas@platonic.systems>" ]
        , text "."
        ]
    , div
        [ class' "m-2" ]
        [ text "Thank you for participating in the political process." ]
    ]

   where
     cfg = config (error "no phantomjs path on client")
     hrefCls = "text-blue-700"


initialPersons :: [PersonalInfo]
initialPersons =
  case multiPersonFeature features of
    MultiPersonFeature -> []
    NoMultiPersonFeature -> [emptyPersonalInfo]


storageKey :: LocalStorageKey [PersonalInfo]
storageKey = LocalStorageKey "testify_personal_info"


getSavedPersonalInfo :: MonadJSM m => m (Maybe [PersonalInfo])
getSavedPersonalInfo = getStorage storageKey


setSavedPersonalInfo :: MonadJSM m => [PersonalInfo] -> m ()
setSavedPersonalInfo = setStorage storageKey


app :: JSM ()
app = do
  addStyle "https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/2.1.0/tailwind.min.css"
  now     <- liftIO getCurrentTime
  tz      <- liftIO getCurrentTimeZone
  persons <- fromMaybe initialPersons <$> getSavedPersonalInfo
  let tomorrow = localDay (utcToLocalTime tz (addUTCTime nominalDay now))
      initialModel = initialViewModel tomorrow persons
  model <- newTVarIO initialModel
  withDeveloperTools model
  shpadoinkle runUIM runParDiff model view getBody


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = do
  putStrLn "\nHappy point of view on https://localhost:8080\n"
  runJSorWarp 8080 app
