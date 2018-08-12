module Example.RealWorld.Data.Group where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Example.App.Validation (class ToText, FieldError)
import Example.RealWorld.Data.Options (Options)
import Formless as F

-----
-- A custom ID type

newtype GroupId = GroupId Int
derive instance newtypeFBGroupId :: Newtype GroupId _
derive newtype instance eqGroupId :: Eq GroupId
derive newtype instance showGroupId :: Show GroupId

-----
-- A nested field type

newtype Admin = Admin { id :: Maybe GroupId }
derive instance newtypeAdmin :: Newtype Admin _
derive newtype instance eqAdmin :: Eq Admin
derive newtype instance showAdmin :: Show Admin

instance toTextAdmin :: ToText Admin where
  toText (Admin { id }) = case id of
    Just (GroupId n) -> "Administrator " <> show n
    Nothing -> "None"

-----
-- Our primary data type
type GroupRow f r =
  ( name         :: f FieldError String         String
  , admin        :: f FieldError (Maybe Admin)  Admin
  , applications :: f FieldError (Array String) (Array String)
  , pixels       :: f FieldError (Array String) (Array String)
  , whiskey      :: f FieldError (Maybe String) String
  | r
  )

-- | Here's the Group data type we'll use throughout our application. After we send
-- | a form result off to the server, this is what we'll get in return.
newtype Group = Group
  ( Record
    ( GroupRow F.OutputType
      ( id :: GroupId
      , secretKey :: String
      , options :: Maybe Options
      )
    )
  )
derive instance newtypeGroup :: Newtype Group _
derive newtype instance eqGroup :: Eq Group
derive newtype instance showGroup :: Show Group

_id = SProxy :: SProxy "id"
_secretKey = SProxy :: SProxy "secretKey"
_options = SProxy :: SProxy "options"

-- | Here's the Form type we'll use to run with Formless.
newtype GroupForm r f = GroupForm (r (GroupFormRow f))
derive instance newtypeGroupForm :: Newtype (GroupForm r f) _

prx :: F.SProxies GroupForm
prx = F.mkSProxies $ F.FormProxy :: F.FormProxy GroupForm

-- | In order to generate our fields automatically using mkFormSpecFromRow, we'll make
-- | sure to have the new row as a new type.
type GroupFormRow f = GroupRow f
  ( secretKey1 :: f FieldError String String
  , secretKey2 :: f FieldError String String
  )
