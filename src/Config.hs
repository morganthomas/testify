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
    , senateFormUrl :: SenateFormUrl
    }
  deriving (Eq, Read, Show)


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
  , senateFormUrl = "http://gencourt.state.nh.us/remotecommittee/senate.aspx"
  }
