-- | A data type representing some large object that will need to be
-- | parsed from a form and sent to a database via JSON. Similarly,
-- | loading up a form ought to load the last-saved values for each
-- | field and hydrate them.
module Example.RealWorld.Data.Group where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Example.RealWorld.Data.Options (Options)
import Example.Validation.Semigroup (Errs)
import Formless.Spec as FSpec

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

-----
-- Our primary data type

-- | We'll define our data type as an extensible row. This represents all
-- | the fields that will be shared between our Form data type and our
-- | resulting Group data type.
-- |
-- | Next, we'll define both data types by extending this row in various
-- | ways. The Group type has an ID key and an options object that the form
-- | won't know about when being filled out. And the Form will have two
-- | secret key fields (one for confirmation) whereas the Group type will
-- | have only one, and of a different name altogether.
-- |
-- | In order to be able to define a form, you'll need to leave a type variable
-- | free that takes 3 types as arguments: the input (what the user provides),
-- | error type (produced by validation), and output (what results from
-- | successful validation).
-- |
-- | In order for this row to be extensible, you'll need another type variable open
-- | to take possible additional rows
-- |                  Input          Error Output
type GroupRow f r =
  ( name         :: f String         Errs String
  , admin        :: f (Maybe Admin)  Errs Admin
  , applications :: f (Array String) Errs (Array String)
  , pixels       :: f (Array String) Errs (Array String)
  , maxBudget    :: f String         Errs (Maybe Int)
  , minBudget    :: f String         Errs Int
  , whiskey      :: f (Maybe String) Errs String
  | r
  )

-- | We'll define proxies for convenience so we can refer to these fields via
-- | lenses and other sorts of things.
_name = SProxy :: SProxy "name"
_admin = SProxy :: SProxy "admin"
_applications = SProxy :: SProxy "applications"
_pixels = SProxy :: SProxy "pixels"
_maxBudget = SProxy :: SProxy "maxBudget"
_minBudget = SProxy :: SProxy "minBudget"
_whiskey = SProxy :: SProxy "whiskey"

-- | Here's the Group data type we'll use throughout our application. After we send
-- | a form result off to the server, this is what we'll get in return on
-- | subsequent form edits.
-- |
-- | Note that we're using the same GroupRow type, but we're filling in `f` with the
-- | Output type provided in the row (the third type on each line), and we're
-- | providing a few new rows.
-- |
-- | Also note that the Group data type relies on another form -- the Options type
-- | from the Options form. With this sort of nesting, we'll use two instances of
-- | Formless.
newtype Group = Group
  ( Record
    ( GroupRow FSpec.Output
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

-- | Here's the Form type we'll use to run with Formless. Again most of the fields are
-- | the same, but just like the Group data type we'll add a few more rows. Unlike that
-- | data type, we need to leave the `f` free for Formless to leverage.
newtype GroupForm f = GroupForm (Record (GroupFormRow f))
derive instance newtypeGroupForm :: Newtype (GroupForm f) _

-- | In order to generate our fields automatically using mkFormSpecFromRow, we'll make
-- | sure to have the new row as a new type.
type GroupFormRow f = GroupRow f
  ( secretKey1 :: f String Errs String
  , secretKey2 :: f String Errs String
  )

_secretKey1 = SProxy :: SProxy "secretKey1"
_secretKey2 = SProxy :: SProxy "secretKey2"
