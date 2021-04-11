{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}


module Automate 
  ( getBills
  , testifyOnBills
  ) where


import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day, toGregorian)
import Test.WebDriver (WD)
import Test.WebDriver.Class
import Test.WebDriver.Commands

import qualified Data.Map as Map
import qualified Data.Set as Set

import Config
import Types


testifyOnBills :: MonadIO m => WebDriver m => Config -> Submission -> m ()
testifyOnBills cfg subm =
  let posMap :: Map Committee (Map Bill (Maybe Position))
      posMap = unPositions $ positions subm

      people :: [PersonalInfo]
      people = persons subm

      day :: Day
      day = submissionDate subm

      chamber :: Chamber
      chamber = submissionChamber subm

      forms :: [(Committee, Bill, Position, PersonalInfo)]
      forms = do
        (c, bills) <- Map.toList posMap
        (bill, mpos) <- Map.toList bills
        pos <- maybeToList mpos
        person <- people
        return (c, bill, pos, person)

  in forM_ forms (\(c,bill,pos,person) -> testifyOnBill cfg day chamber c bill pos person)


getFormUrl :: Config -> Chamber -> Text
getFormUrl cfg House = unHouseFormUrl (houseFormUrl cfg)
getFormUrl cfg Senate = unSenateFormUrl (senateFormUrl cfg)


testifyOnBill :: MonadIO m => WebDriver m => Config -> Day -> Chamber -> Committee -> Bill -> Position -> PersonalInfo -> m ()
testifyOnBill cfg day chamber committee bill position person = do
  openPage (unpack (getFormUrl cfg chamber))
  dayEl <- waitForElem (daySelector day)
  click dayEl
  committeeSelect <- waitForElem . ByCSS . unCommitteeDropdownSelector $ committeeDropdownSelector cfg
  click committeeSelect
  wait
  committeeEl <- findElem (getCommitteeSelector cfg committee)
  click committeeEl
  wait
  billSelect <- findElem . ByCSS . unBillDropdownSelector $ billDropdownSelector cfg
  click billSelect
  billEl <- findElem (billSelector cfg bill)
  click billEl
  wait
  iamSelect <- findElem . ByCSS . unIAmDropdownSelector $ iAmDropdownSelector cfg
  click iamSelect
  iamEl <- findElem . ByCSS . unIAmOptionSelector $ iAmOptionSelector cfg
  click iamEl
  wait
  posEl <- case position of
             Support -> findElem . ByCSS . unSupportSelector $ supportSelector cfg
             Oppose -> findElem . ByCSS . unOpposeSelector $ opposeSelector cfg
             Neutral -> findElem . ByCSS . unNeutralSelector $ neutralSelector cfg
  click posEl
  wait
  continueEl <- findElem . ByCSS . unContinueSelector $ continueSelector cfg
  continueDisabled <- attr continueEl "disabled"
  liftIO $ putStrLn (show continueDisabled)
  when (continueDisabled == Just "disabled") (error "continue is disabled")
  click continueEl
  firstNameEl <- waitForElem . ByCSS . unFirstNameSelector $ firstNameSelector cfg
  sendKeys (unFirstName (firstName person)) firstNameEl
  lastNameEl <- findElem . ByCSS . unLastNameSelector $ lastNameSelector cfg
  sendKeys (unLastName (lastName person)) lastNameEl
  emailEl <- findElem . ByCSS . unEmailSelector $ emailSelector cfg
  sendKeys (unEmail (email person)) emailEl
  case chamber of
    Senate -> return ()
    House -> do
      townEl <- findElem . ByCSS . unHouseTownSelector $ houseTownSelector cfg
      sendKeys (unTown (town person)) townEl
  continueEl2 <- findElem . ByCSS . unContinueSelector2 $ continueSelector2 cfg
  click continueEl2
  agreeEl <- waitForElem . ByCSS . unAgreeSelector $ agreeSelector cfg
  click agreeEl
  continueEl3 <- findElem . ByCSS . unContinueSelector3 $ continueSelector3 cfg
  click continueEl3


wait :: MonadIO m => m ()
wait = liftIO $ threadDelay 500000


waitForElem :: MonadIO m => WebDriver m => Selector -> m Element
waitForElem selector = do
  els <- findElems selector
  case els of
    []   -> wait >> waitForElem selector
    [el] -> return el
    _    -> error "element is not unique"


getBills :: MonadIO m => WebDriver m => Config -> Day -> Chamber -> m Agenda
getBills cfg day chamber = do
  openPage (unpack (getFormUrl cfg chamber))
  dayEl <- waitForElem (daySelector day)
  click dayEl
  wait
  committees <- getCommittees cfg
  Agenda . Map.fromList <$> forM committees (\c -> (c,) . Set.fromList <$> getCommitteeBills cfg c)


getCommittees :: MonadIO m => WebDriver m => Config -> m [Committee]
getCommittees cfg = do
  els <- findElems . ByCSS . unCommitteeSelector $ committeeSelector cfg
  committeeNames <- fmap CommitteeName <$> forM els getText
  committeeIds <- fmap (CommitteeId . fromMaybe "0") <$> forM els (\el -> attr el "value")
  return . filter (\cm -> committeeName cm /= unSelectCommitteeOption (selectCommitteeOption cfg))
    $ zipWith Committee committeeNames committeeIds


getCommitteeBills :: MonadIO m => WebDriver m => Config -> Committee -> m [Bill]
getCommitteeBills cfg committee = do
  select <- findElem . ByCSS . unCommitteeDropdownSelector $ committeeDropdownSelector cfg
  click select
  option <- waitForElem $ getCommitteeSelector cfg committee
  click option
  _ <- waitForElem . ByCSS $ unBillDropdownSelector (billDropdownSelector cfg) <> " option[selected=\"selected\"]"
  billEls <- findElems . ByCSS $ unBillDropdownSelector (billDropdownSelector cfg)
                              <> " option:not(selected)"
  billNames <- fmap BillName <$> forM billEls getText
  billIds <- fmap (BillId . fromMaybe "0") <$> forM billEls (\e -> attr e "value")
  return . filter (\bill -> billName bill /= unSelectBillOption (selectBillOption cfg))
    $ zipWith Bill billNames billIds


getCommitteeSelector :: Config -> Committee -> Selector
getCommitteeSelector cfg committee = ByCSS $ unCommitteeDropdownSelector (committeeDropdownSelector cfg)
                                       <> " option[value=" <> pack (show (unCommitteeId (committeeId committee))) <> "]"


billSelector :: Config -> Bill -> Selector
billSelector cfg bill = ByCSS $ unBillDropdownSelector (billDropdownSelector cfg)
                             <> " option[value=" <> pack (show (unBillId (billId bill))) <> "]"


daySelector :: Day -> Selector
daySelector day =
  let (_, month, dayOfMonth) = toGregorian day
      monthText = monthToText month
      leadingZero = if dayOfMonth < 10 then "0" else ""
  in ByCSS $ "a[title=\"" <> monthText <> " " <> leadingZero <> pack (show dayOfMonth) <> "\"]"


monthToText :: Int -> Text
monthToText 1 = "January"
monthToText 2 = "February"
monthToText 3 = "March"
monthToText 4 = "April"
monthToText 5 = "May"
monthToText 6 = "June"
monthToText 7 = "July"
monthToText 8 = "August"
monthToText 9 = "September"
monthToText 10 = "October"
monthToText 11 = "November"
monthToText 12 = "December"
monthToText _ = "Septembruary"
