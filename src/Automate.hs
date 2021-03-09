{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}


module Automate 
  ( getHouseBills
  , testifyOnHouseBills
  ) where


import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_)
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


testifyOnHouseBills :: MonadIO m => WebDriver m => Config -> Submission -> m ()
testifyOnHouseBills cfg subm =
  let posMap :: Map Committee (Map Bill (Maybe Position))
      posMap = unPositions $ positions subm

      people :: [PersonalInfo]
      people = persons subm

      day :: Day
      day = submissionDate subm

      forms :: [(Committee, Bill, Position, PersonalInfo)]
      forms = do
        (c, bills) <- Map.toList posMap
        (bill, mpos) <- Map.toList bills
        pos <- maybeToList mpos
        person <- people
        return (c, bill, pos, person)

  in forM_ forms (\(c,bill,pos,person) -> testifyOnHouseBill cfg day c bill pos person)


testifyOnHouseBill :: MonadIO m => WebDriver m => Config -> Day -> Committee -> Bill -> Position -> PersonalInfo -> m ()
testifyOnHouseBill cfg day committee bill position person = do
  openPage (unpack (unHouseFormUrl (houseFormUrl cfg)))
  dayEl <- waitForElem (daySelector day)
  click dayEl
  wait
  committeeSelect <- findElem . ByCSS . unHouseCommitteeDropdownSelector $ houseCommitteeDropdownSelector cfg
  click committeeSelect
  wait
  committeeEl <- findElem (committeeSelector cfg committee)
  click committeeEl
  wait
  billSelect <- findElem . ByCSS . unHouseBillDropdownSelector $ houseBillDropdownSelector cfg
  click billSelect
  billEl <- findElem (billSelector cfg bill)
  click billEl
  iamSelect <- findElem . ByCSS . unHouseIAmDropdownSelector $ houseIAmDropdownSelector cfg
  click iamSelect
  iamEl <- findElem . ByCSS . unHouseIAmOptionSelector $ houseIAmOptionSelector cfg
  click iamEl
  posEl <- case position of
             Support -> findElem . ByCSS . unHouseSupportSelector $ houseSupportSelector cfg
             Oppose -> findElem . ByCSS . unHouseOpposeSelector $ houseOpposeSelector cfg
             Neutral -> findElem . ByCSS . unHouseNeutralSelector $ houseNeutralSelector cfg
  click posEl
  continueEl <- findElem . ByCSS . unHouseContinueSelector $ houseContinueSelector cfg
  click continueEl
  firstNameEl <- waitForElem . ByCSS . unHouseFirstNameSelector $ houseFirstNameSelector cfg
  sendKeys (unFirstName (firstName person)) firstNameEl
  lastNameEl <- findElem . ByCSS . unHouseLastNameSelector $ houseLastNameSelector cfg
  sendKeys (unLastName (lastName person)) lastNameEl
  emailEl <- findElem . ByCSS . unHouseEmailSelector $ houseEmailSelector cfg
  sendKeys (unEmail (email person)) emailEl
  townEl <- findElem . ByCSS . unHouseTownSelector $ houseTownSelector cfg
  sendKeys (unTown (town person)) townEl
  continueEl2 <- findElem . ByCSS . unHouseContinueSelector2 $ houseContinueSelector2 cfg
  click continueEl2
  agreeEl <- waitForElem . ByCSS . unHouseAgreeSelector $ houseAgreeSelector cfg
  click agreeEl
  continueEl3 <- findElem . ByCSS . unHouseContinueSelector3 $ houseContinueSelector3 cfg
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


getHouseBills :: MonadIO m => WebDriver m => Config -> Day -> m Agenda
getHouseBills cfg day = do
  openPage (unpack (unHouseFormUrl (houseFormUrl cfg)))
  dayEl <- waitForElem (daySelector day)
  click dayEl
  wait
  committees <- getHouseCommittees cfg
  Agenda . Map.fromList <$> forM committees (\c -> (c,) . Set.fromList <$> getHouseCommitteeBills cfg c)


getHouseCommittees :: MonadIO m => WebDriver m => Config -> m [Committee]
getHouseCommittees cfg = do
  els <- findElems . ByCSS . unHouseCommitteeSelector $ houseCommitteeSelector cfg
  committeeNames <- fmap CommitteeName <$> forM els getText
  committeeIds <- fmap (CommitteeId . fromMaybe "0") <$> forM els (\el -> attr el "value")
  return . filter (\cm -> committeeName cm /= unHouseSelectCommitteeOption (houseSelectCommitteeOption cfg))
    $ zipWith Committee committeeNames committeeIds


getHouseCommitteeBills :: MonadIO m => WebDriver m => Config -> Committee -> m [Bill]
getHouseCommitteeBills cfg committee = do
  select <- findElem . ByCSS . unHouseCommitteeDropdownSelector $ houseCommitteeDropdownSelector cfg
  click select
  option <- waitForElem $ committeeSelector cfg committee
  click option
  wait
  billEls <- findElems . ByCSS $ unHouseBillDropdownSelector (houseBillDropdownSelector cfg)
                              <> " option:not(selected)"
  billNames <- fmap BillName <$> forM billEls getText
  billIds <- fmap (BillId . fromMaybe "0") <$> forM billEls (\e -> attr e "value")
  return . filter (\bill -> billName bill /= unHouseSelectBillOption (houseSelectBillOption cfg))
    $ zipWith Bill billNames billIds


committeeSelector :: Config -> Committee -> Selector
committeeSelector cfg committee = ByCSS $ unHouseCommitteeDropdownSelector (houseCommitteeDropdownSelector cfg)
                                       <> " option[value=" <> pack (show (unCommitteeId (committeeId committee))) <> "]"


billSelector :: Config -> Bill -> Selector
billSelector cfg bill = ByCSS $ unHouseBillDropdownSelector (houseBillDropdownSelector cfg)
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
