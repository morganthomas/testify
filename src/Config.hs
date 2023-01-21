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


newtype SubmitSelector = SubmitSelector { unSubmitSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype FirstNameSelector = FirstNameSelector { unFirstNameSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype LastNameSelector = LastNameSelector { unLastNameSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype EmailSelector = EmailSelector { unEmailSelector :: Text }
  deriving (Eq, Read, Show, IsString)


newtype TownSelector = TownSelector { unTownSelector :: Text }
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
    , firstNameSelector :: FirstNameSelector
    , lastNameSelector :: LastNameSelector
    , emailSelector :: EmailSelector
    , townSelector :: TownSelector
    , selectCommitteeOption :: SelectCommitteeOption
    , submitSelector :: SubmitSelector
    , agreeSelector :: AgreeSelector
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
  , senateFormUrl = "http://gencourt.state.nh.us/remotecommittee/senate.aspx"
  , committeeSelector = "select#pageBody_ddlCommittee option"
  , committeeDropdownSelector = "select#pageBody_ddlCommittee"
  , billDropdownSelector = "select#pageBody_ddlBills"
  , iAmDropdownSelector = "select#pageBody_ddlWho"
  , iAmOptionSelector = "select#pageBody_ddlWho option[value=\"4\"]"
  , supportSelector = "input#pageBody_rdoPosition_0"
  , opposeSelector = "input#pageBody_rdoPosition_1"
  , neutralSelector = "input#pageBody_rdoPosition_2"
  , firstNameSelector = "input#pageBody_txtFirstName"
  , lastNameSelector = "input#pageBody_txtLastName"
  , emailSelector = "input#pageBody_txtEmail"
  , townSelector = "input#pageBody_txtTown"
  , selectCommitteeOption = "Select a Committee -->"
  , selectBillOption = "Select a Bill -->"
  , submitSelector = "input#pageBody_btnContinue"
  , agreeSelector = "input#pageBody_chkAgree"
  }
