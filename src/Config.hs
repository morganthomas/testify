{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Config where


import Data.String (IsString)
import Data.Text (Text)

import Types


newtype HouseFormUrl = HouseFormUrl { unHouseFormUrl :: Text }
  deriving (Eq, Read, Show, IsString)


newtype CommitteeSelector = CommitteeSelector { unCommitteeSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype CommitteeDropdownSelector = CommitteeDropdownSelector { unCommitteeDropdownSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype BillDropdownSelector = BillDropdownSelector { unBillDropdownSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype IAmDropdownSelector = IAmDropdownSelector { unIAmDropdownSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype IAmOptionSelector = IAmOptionSelector { unIAmOptionSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype SupportSelector = SupportSelector { unSupportSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype OpposeSelector = OpposeSelector { unOpposeSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype NeutralSelector = NeutralSelector { unNeutralSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype ContinueSelector = ContinueSelector { unContinueSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype FirstNameSelector = FirstNameSelector { unFirstNameSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype LastNameSelector = LastNameSelector { unLastNameSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype EmailSelector = EmailSelector { unEmailSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype HouseTownSelector = HouseTownSelector { unHouseTownSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype ContinueSelector2 = ContinueSelector2 { unContinueSelector2 :: Text }
  deriving (Eq, Read, Show, IsString)


newtype AgreeSelector = AgreeSelector { unAgreeSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype ContinueSelector3 = ContinueSelector3 { unContinueSelector3 :: Text }
  deriving (Eq, Read, Show, IsString)


newtype SelectCommitteeOption = SelectCommitteeOption { unSelectCommitteeOption :: CommitteeName }
  deriving (Eq, Read, Show, IsString)


newtype SelectBillOption = SelectBillOption { unSelectBillOption :: BillName }
  deriving (Eq, Read, Show, IsString)


newtype SenateFormUrl = SenateFormUrl { unSenateFormUrl :: Text }
  deriving (Eq, Read, Show, IsString)


newtype UserAgentString = UserAgentString { unUserAgentString :: Text }
  deriving (Eq, Read, Show, IsString)


newtype PhantomjsPath = PhantomjsPath { unPhantomjsPath :: String }
  deriving (Eq, Read, Show, IsString)


data Config
  = Config
    { userAgentString :: UserAgentString
    , phantomjsPath :: PhantomjsPath
    , houseFormUrl :: HouseFormUrl
    , committeeSelector :: CommitteeSelector
    , committeeDropdownSelector :: CommitteeDropdownSelector
    , billDropdownSelector :: BillDropdownSelector
    , iAmDropdownSelector :: IAmDropdownSelector
    , iAmOptionSelector :: IAmOptionSelector
    , supportSelector :: SupportSelector
    , opposeSelector :: OpposeSelector
    , neutralSelector :: NeutralSelector
    , continueSelector :: ContinueSelector
    , firstNameSelector :: FirstNameSelector
    , lastNameSelector :: LastNameSelector
    , emailSelector :: EmailSelector
    , houseTownSelector :: HouseTownSelector
    , continueSelector2 :: ContinueSelector2
    , agreeSelector :: AgreeSelector
    , continueSelector3 :: ContinueSelector3
    , selectCommitteeOption :: SelectCommitteeOption
    , selectBillOption :: SelectBillOption
    , senateFormUrl :: SenateFormUrl
    }
  deriving (Eq, Read, Show)


class HasConfig m where
  getConfig :: m Config


config :: PhantomjsPath -> Config
config phantomPath =
  Config
  { userAgentString = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.66 Safari/537.36"
  , phantomjsPath = phantomPath
  , houseFormUrl = "http://gencourt.state.nh.us/house/committees/remotetestimony/default.aspx"
  , committeeSelector = "select[name=\"ddlCommittee\"] option"
  , committeeDropdownSelector = "select[name=\"ddlCommittee\"]"
  , billDropdownSelector = "select[name=\"ddlBills\"]"
  , iAmDropdownSelector = "select[name=\"ddlWho\"]"
  , iAmOptionSelector = "select[name=\"ddlWho\"] option[value=\"4\"]"
  , supportSelector = "#rdoPosition_0"
  , opposeSelector = "#rdoPosition_1"
  , neutralSelector = "#rdoPosition_2"
  , continueSelector = "#btnContinue"
  , firstNameSelector = "#txtFirstName"
  , lastNameSelector = "#txtLastName"
  , emailSelector = "#txtEmail"
  , houseTownSelector = "#txtTown"
  , continueSelector2 = "#btnContinue"
  , agreeSelector = "#chkAgree"
  , continueSelector3 = "#btnContinue"
  , selectCommitteeOption = "Select a Committee -->"
  , selectBillOption = "Select a Bill -->"
  , senateFormUrl = "http://gencourt.state.nh.us/remotecommittee/senate.aspx"
  }
