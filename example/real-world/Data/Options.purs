-- | A data type representing some large object that will need to be
-- | parsed from a form and sent to a database via JSON. Similarly,
-- | loading up a form ought to load the last-saved values for each
-- | field and hydrate them.
module Example.RealWorld.Data.Options where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Example.App.Validation (class ToText, FieldError)
import Formless as F

-----
-- Some custom data

-- | This data type represents dollar amounts
newtype Dollars = Dollars Int
derive instance newtypeDollars :: Newtype Dollars _
derive newtype instance eqDollars :: Eq Dollars
derive newtype instance showDollars :: Show Dollars

-- | This data type represents different metrics a user
-- | can choose from. Depending on what metric they choose,
-- | only fields relevant to that metric ought to render in
-- | the form.
data Metric
  = ViewCost
  | ClickCost
  | InstallCost
derive instance genericMetric :: Generic Metric _
derive instance eqMetric :: Eq Metric
derive instance ordMetric :: Ord Metric

instance showMetric :: Show Metric where
  show = genericShow

instance toTextMetric :: ToText Metric where
  toText ViewCost = "View Cost"
  toText ClickCost = "Click Cost"
  toText InstallCost = "Install Cost"

-- | This data type will be used in radio buttons, and so if we
-- | want to generate an initial form from our row, we'll need an
-- | instance of the F.Initial type class
data Speed
  = Low
  | Medium
  | Fast
derive instance genericSpeed :: Generic Speed _
derive instance eqSpeed :: Eq Speed
derive instance ordSpeed :: Ord Speed

instance showSpeed :: Show Speed where
  show = genericShow

instance initialSpeed :: F.Initial Speed where
  initial = Low

-----
-- Our primary data type

-- | Just like the Group data type, we'll use a row as our underlying type. However,
-- | since we don't have to extend this with any fields, we can stick with a simpler
-- | closed row. In the case of the 'enable' option, we know there's no validation
-- | for it, so we'll use `Void` as the error type.
type OptionsRow f =
  ( enable       :: f Void       Boolean        Boolean
  , metric       :: f FieldError (Maybe Metric) Metric
  , viewCost     :: f FieldError String         (Maybe Dollars)
  , clickCost    :: f FieldError String         (Maybe Dollars)
  , installCost  :: f FieldError String         (Maybe Dollars)
  , size         :: f FieldError String         Number
  , dimensions   :: f FieldError String         Number
  , speed        :: f Void       Speed          Speed
  )

prx :: F.SProxies OptionsForm
prx = F.mkSProxies $ F.FormProxy :: F.FormProxy OptionsForm

-- | This is the data type used throughout the application. In this case, it's the same
-- | as the form and the underlying row.
newtype Options = Options (Record (OptionsRow F.OutputType))
derive instance newtypeOptions :: Newtype Options _
derive newtype instance eqOptions :: Eq Options
derive newtype instance showOptions :: Show Options

-- | Here's the Form type we'll use to run with Formless. The fields are the same as the
-- | underlying row.
newtype OptionsForm r f = OptionsForm (r (OptionsRow f))
derive instance newtypeOptionsForm :: Newtype (OptionsForm r f) _
