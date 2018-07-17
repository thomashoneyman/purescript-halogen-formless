-- | A data type representing some large object that will need to be
-- | parsed from a form and sent to a database via JSON. Similarly,
-- | loading up a form ought to load the last-saved values for each
-- | field and hydrate them.
module Example.RealWorld.Data.Options where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Read (class Read)
import Data.Symbol (SProxy(..))
import Example.Validation.Semigroup (Errs)
import Formless.Class.Initial (class Initial)
import Formless.Spec as FSpec

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

instance readMetric :: Read Metric where
  read "ViewCost" = Just ViewCost
  read "ClickCost" = Just ClickCost
  read "InstallCost" = Just InstallCost
  read _ = Nothing

-- | This data type will be used in radio buttons, and so if we
-- | want to generate an initial form from our row, we'll need an
-- | instance of the Initial type class
data Speed
  = Low
  | Medium
  | Fast
derive instance genericSpeed :: Generic Speed _
derive instance eqSpeed :: Eq Speed
derive instance ordSpeed :: Ord Speed

instance showSpeed :: Show Speed where
  show = genericShow

instance initialSpeed :: Initial Speed where
  initial = Low

-----
-- Our primary data type

-- | Just like the Group data type, we'll use a row as our underlying type. However,
-- | since we don't have to extend this with any fields, we can stick with a simpler
-- | closed row. In the case of the 'enable' option, we know there's no validation
-- | for it, so we'll use `Void` as the error type.
type OptionsRow f =
  ( enable       :: f Boolean        Void Boolean
  , metric       :: f (Maybe Metric) Errs Metric
  , viewCost     :: f String         Errs (Maybe Dollars)
  , clickCost    :: f String         Errs (Maybe Dollars)
  , installCost  :: f String         Errs (Maybe Dollars)
  , size         :: f String         Errs Number
  , dimensions   :: f String         Errs Number
  , speed        :: f Speed          Void Speed
  )

_enable = SProxy :: SProxy "enable"
_metric = SProxy :: SProxy "metric"
_viewCost = SProxy :: SProxy "viewCost"
_clickCost = SProxy :: SProxy "clickCost"
_installCost = SProxy :: SProxy "installCost"
_size = SProxy :: SProxy "size"
_dimensions = SProxy :: SProxy "dimensions"
_speed = SProxy :: SProxy "speed"

-- | This is the data type used throughout the application. In this case, it's the same
-- | as the form and the underlying row.
newtype Options = Options (Record (OptionsRow FSpec.Output))
derive instance newtypeOptions :: Newtype Options _
derive newtype instance eqOptions :: Eq Options
derive newtype instance showOptions :: Show Options

-- | Here's the Form type we'll use to run with Formless. The fields are the same as the
-- | underlying row.
newtype OptionsForm f = OptionsForm (Record (OptionsRow f))
derive instance newtypeOptionsForm :: Newtype (OptionsForm f) _
