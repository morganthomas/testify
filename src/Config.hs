{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Config where


import Data.String (IsString)
import Data.Text (Text)


newtype HouseFormUrl = HouseFormUrl { unHouseFormUrl :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseCommitteeSelector = HouseCommitteeSelector { unHouseCommitteeSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseCommitteeDropdownSelector = HouseCommitteeDropdownSelector { unHouseCommitteeDropdownSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseBillDropdownSelector = HouseBillDropdownSelector { unHouseBillDropdownSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseIAmDropdownSelector = HouseIAmDropdownSelector { unHouseIAmDropdownSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseIAmOptionSelector = HouseIAmOptionSelector { unHouseIAmOptionSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseSupportSelector = HouseSupportSelector { unHouseSupportSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseOpposeSelector = HouseOpposeSelector { unHouseOpposeSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseContinueSelector = HouseContinueSelector { unHouseContinueSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseFirstNameSelector = HouseFirstNameSelector { unHouseFirstNameSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseLastNameSelector = HouseLastNameSelector { unHouseLastNameSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseEmailSelector = HouseEmailSelector { unHouseEmailSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseContinueSelector2 = HouseContinueSelector2 { unHouseContinueSelector2 :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseAgreeSelector = HouseAgreeSelector { unHouseAgreeSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseContinueSelector3 = HouseContinueSelector3 { unHouseContinueSelector3 :: Text }
  deriving (Eq, Read, Show, IsString)


newtype SenateFormUrl = SenateFormUrl { unSenateFormUrl :: Text }
  deriving (Eq, Read, Show, IsString)


data Config
  = Config
    { houseFormUrl :: HouseFormUrl
    , houseCommitteeSelector :: HouseCommitteeSelector
    , houseCommitteeDropdownSelector :: HouseCommitteeDropdownSelector
    , houseBillDropdownSelector :: HouseBillDropdownSelector
    , houseIAmDropdownSelector :: HouseIAmDropdownSelector
    , houseIAmOptionSelector :: HouseIAmOptionSelector
    , houseSupportSelector :: HouseSupportSelector
    , houseOpposeSelector :: HouseOpposeSelector
    , houseContinueSelector :: HouseContinueSelector
    , houseFirstNameSelector :: HouseFirstNameSelector
    , houseLastNameSelector :: HouseLastNameSelector
    , houseEmailSelector :: HouseEmailSelector
    , houseContinueSelector2 :: HouseContinueSelector2
    , houseAgreeSelector :: HouseAgreeSelector
    , houseContinueSelector3 :: HouseContinueSelector3
    , senateFormUrl :: SenateFormUrl
    }
  deriving (Eq, Read, Show)


class HasConfig m where
  getConfig :: m Config


config :: Config
config =
  Config
  { houseFormUrl = "http://gencourt.state.nh.us/house/committees/remotetestimony/default.aspx"
  , houseCommitteeSelector = "select[name=\"ddlCommittee\"] option"
  , houseCommitteeDropdownSelector = "select[name=\"ddlCommittee\"]"
  , houseBillDropdownSelector = "select[name=\"ddlBills\"]"
  , houseIAmDropdownSelector = "select[name=\"ddlWho\"]"
  , houseIAmOptionSelector = "select[name=\"ddlWho\"] option[value=\"4\"]"
  , houseSupportSelector = "#rdoPosition_0"
  , houseOpposeSelector = "#rdoPosition_1"
  , houseContinueSelector = "#btnContinue"
  , houseFirstNameSelector = "#txtFirstName"
  , houseLastNameSelector = "#txtLastName"
  , houseEmailSelector = "#txtEmail"
  , houseContinueSelector2 = "#btnContinue"
  , houseAgreeSelector = "#chkAgree"
  , houseContinueSelector3 = "#btnContinue"
  , senateFormUrl = "http://gencourt.state.nh.us/remotecommittee/senate.aspx"
  }
