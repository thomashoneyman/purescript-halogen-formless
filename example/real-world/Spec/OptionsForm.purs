module Example.RealWorld.Spec.OptionsForm where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Example.App.Validation as V
import Example.RealWorld.Data.Options (Metric(..), OptionsForm(..), Speed(..))
import Formless as F
import Formless.Validation.Semigroup (toEitherPure)

optionsFormSpec :: ∀ m. Monad m => OptionsForm Record (F.FormSpec m)
optionsFormSpec = OptionsForm
  { enable: F.FormSpec
    { input: false
    , validator: toEitherPure pure
    }
  , metric: F.FormSpec
    { input: Nothing
    , validator: toEitherPure V.validateMaybe
    }
  , viewCost: F.FormSpec
    { input: ""
    , validator: toEitherPure (const $ pure Nothing)
    }
  , clickCost: F.FormSpec
    { input: ""
    , validator: toEitherPure (const $ pure Nothing)
    }
  , installCost: F.FormSpec
    { input: ""
    , validator: toEitherPure (const $ pure Nothing)
    }
  , size: F.FormSpec
    { input: ""
    , validator: toEitherPure validateInt
    }
  , dimensions: F.FormSpec
    { input: ""
    , validator: toEitherPure validateInt
    }
  , speed: F.FormSpec
    { input: Medium
    , validator: toEitherPure pure
    }
  }
  where
    validateInt str = Int.toNumber <$> V.validateInt str


-- In the case the user has not toggled the options on, we'll provide them with
-- valid default values.
--
-- TODO: Surely this could be better.
defaultOptionsSpec :: ∀ m. Monad m => OptionsForm Record (F.FormSpec m)
defaultOptionsSpec = OptionsForm $ spec
  { metric = over F.FormSpec (_ { input = Just ViewCost }) spec.metric
  , viewCost = over F.FormSpec (_ { input = "1" }) spec.viewCost
  , size = over F.FormSpec (_ { input = "21" }) spec.size
  , dimensions = over F.FormSpec (_ { input = "3005" }) spec.dimensions
  }
  where
    spec = unwrap optionsFormSpec

--  TODO: Dependent metric validation

--  optionsFormValidate
--    :: OptionsForm Record F.FormField
--    -> OptionsForm Record F.FormField
--  optionsFormValidate (OptionsForm form) = OptionsForm
--    { enable: pure `onFormField` form.enable
--    , metric: V.validateMaybe `onFormField` form.metric
--    , viewCost: (validateMetric ViewCost) `onFormField` form.viewCost
--    , clickCost: (validateMetric ClickCost) `onFormField` form.clickCost
--    , installCost: (validateMetric InstallCost) `onFormField` form.installCost
--    , size: validateInt `onFormField` form.size
--    , dimensions: validateInt `onFormField` form.dimensions
--    , speed: pure `onFormField` form.speed
--    }
--    where
--      metric = F.getInput prx.metric (OptionsForm form)
--
--      validateMetric m str
--        | metric == Just m = pure <<< Dollars <$> V.validateInt str
--        | otherwise = pure Nothing
--
