{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Automate 
  ( getHouseBills
  , testifyOnHouseBill
  ) where


import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day, toGregorian)
import Test.WebDriver.Class
import Test.WebDriver.Commands

import qualified Data.Map as Map
import qualified Data.Set as Set

import Config
import Types


testifyOnHouseBill :: WebDriver m => Config -> Day -> Committee -> Bill -> Position -> PersonalInfo -> m ()
testifyOnHouseBill cfg day committee bill position person = do
  openPage (unpack (unHouseFormUrl (houseFormUrl cfg)))
  dayEl <- findElem (daySelector day)
  click dayEl
  committeeSelect <- findElem . ByCSS . unHouseCommitteeDropdownSelector $ houseCommitteeDropdownSelector cfg
  click committeeSelect
  committeeEl <- findElem (committeeSelector cfg committee)
  click committeeEl
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
  click posEl
  continueEl <- findElem . ByCSS . unHouseContinueSelector $ houseContinueSelector cfg
  click continueEl
  firstNameEl <- findElem . ByCSS . unHouseFirstNameSelector $ houseFirstNameSelector cfg
  sendKeys (unFirstName (firstName person)) firstNameEl
  lastNameEl <- findElem . ByCSS . unHouseLastNameSelector $ houseLastNameSelector cfg
  sendKeys (unLastName (lastName person)) lastNameEl
  emailEl <- findElem . ByCSS . unHouseEmailSelector $ houseEmailSelector cfg
  sendKeys (unEmail (email person)) emailEl
  continueEl2 <- findElem . ByCSS . unHouseContinueSelector2 $ houseContinueSelector2 cfg
  click continueEl2
  agreeEl <- findElem . ByCSS . unHouseAgreeSelector $ houseAgreeSelector cfg
  click agreeEl
  continueEl3 <- findElem . ByCSS . unHouseContinueSelector3 $ houseContinueSelector3 cfg
  click continueEl3


getHouseBills :: WebDriver m => Config -> Day -> m Agenda
getHouseBills cfg day = do
  openPage (unpack (unHouseFormUrl (houseFormUrl cfg)))
  dayEl <- findElem (daySelector day)
  click dayEl
  committees <- getHouseCommittees cfg
  Agenda . Map.fromList <$> forM committees (\c -> (c,) . Set.fromList <$> getHouseCommitteeBills cfg c)


getHouseCommittees :: WebDriver m => Config -> m [Committee]
getHouseCommittees cfg = do
  els <- findElems . ByCSS . unHouseCommitteeSelector $ houseCommitteeSelector cfg
  committeeNames <- fmap CommitteeName <$> forM els getText
  committeeIds <- fmap (CommitteeId . fromMaybe "0") <$> forM els (\el -> attr el "value")
  return (zipWith Committee committeeNames committeeIds)


getHouseCommitteeBills :: WebDriver m => Config -> Committee -> m [Bill]
getHouseCommitteeBills cfg committee = do
  select <- findElem . ByCSS . unHouseCommitteeDropdownSelector $ houseCommitteeDropdownSelector cfg
  click select
  option <- findElem $ committeeSelector cfg committee
  click option
  billEls <- findElems . ByCSS $ unHouseBillDropdownSelector (houseBillDropdownSelector cfg)
                              <> " option:not(selected)"
  billNames <- fmap BillName <$> forM billEls getText
  billIds <- fmap (BillId . fromMaybe "0") <$> forM billEls (\e -> attr e "value")
  return (zipWith Bill billNames billIds)


committeeSelector :: Config -> Committee -> Selector
committeeSelector cfg committee = ByCSS $ unHouseCommitteeDropdownSelector (houseCommitteeDropdownSelector cfg)
                                       <> " option[value=\"" <> pack (show (unCommitteeId (committeeId committee))) <> "\"]"


billSelector :: Config -> Bill -> Selector
billSelector cfg bill = ByCSS $ unHouseBillDropdownSelector (houseBillDropdownSelector cfg)
                             <> " option[value=\"" <> pack (show (unBillId (billId bill))) <> "\"]"


daySelector :: Day -> Selector
daySelector day =
  let (_, month, dayOfMonth) = toGregorian day
      monthText = monthToText month
  in ByCSS $ "a[title=\"" <> monthText <> " " <> pack (show dayOfMonth) <> "\"]"


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
