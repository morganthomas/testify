{-# LANGUAGE OverloadedStrings #-}


module Automate where


import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day, toGregorian)
import Test.WebDriver.Class
import Test.WebDriver.Commands

import Config
import Types


getHouseBills :: WebDriver m => Config -> Day -> m Agenda
getHouseBills cfg day = do
  openPage (unpack (unHouseFormUrl (houseFormUrl cfg)))
  dayEl <- findElem (daySelector day)
  click dayEl
  committees <- getCommittees cfg
  return (error "todo")


getCommittees :: WebDriver m => Config -> m [Committee]
getCommittees cfg = do
  els <- findElems . ByCSS . unHouseCommitteeSelector $ houseCommitteeSelector cfg
  committeeNames <- fmap CommitteeName <$> forM els getText
  committeeIds <- fmap (CommitteeId . fromMaybe "0") <$> forM els (\el -> attr el "value")
  return (zipWith Committee committeeNames committeeIds)


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
