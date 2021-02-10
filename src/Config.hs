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


newtype SenateFormUrl = SenateFormUrl { unSenateFormUrl :: Text }
  deriving (Eq, Read, Show, IsString)


data Config
  = Config
    { houseFormUrl :: HouseFormUrl
    , houseCommitteeSelector :: HouseCommitteeSelector
    , houseCommitteeDropdownSelector :: HouseCommitteeDropdownSelector
    , senateFormUrl :: SenateFormUrl
    }
  deriving (Eq, Read, Show)


config :: Config
config =
  Config
  { houseFormUrl = "http://gencourt.state.nh.us/house/committees/remotetestimony/default.aspx"
  , houseCommitteeSelector = "select[name=\"ddlCommittee\"] option"
  , houseCommitteeDropdownSelector = "select[name=\"ddlCommittee\"]"
  , senateFormUrl = "http://gencourt.state.nh.us/remotecommittee/senate.aspx"
  }