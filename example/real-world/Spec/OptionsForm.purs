module Example.RealWorld.Spec.OptionsForm where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Example.App.Validation as V
import Example.RealWorld.Data.Options (Dollars(..), Metric(..), OptionsForm(..), prx)
import Formless as F

optionsFormInputs :: OptionsForm Record F.InputField
optionsFormInputs = F.mkInputFields $ F.FormProxy :: F.FormProxy OptionsForm

-- In the case the user has not toggled the options on, we'll provide them with
-- valid default values.
defaultInputs :: OptionsForm Record F.InputField
defaultInputs = OptionsForm $ inputs
  { metric = F.InputField $ Just ViewCost
  , viewCost = F.InputField "1"
  , size = F.InputField "21"
  , dimensions = F.InputField "3005"
  }
  where
    inputs = unwrap optionsFormInputs

optionsFormValidators :: ∀ m. Monad m => OptionsForm Record (F.Validation OptionsForm m)
optionsFormValidators = OptionsForm
  { enable: F.hoistFn_ identity
  , metric: V.exists
  , viewCost: validateMetric ViewCost
  , clickCost: validateMetric ClickCost
  , installCost: validateMetric InstallCost
  , size: Int.toNumber <$> V.strIsInt
  , dimensions: Int.toNumber <$> V.strIsInt
  , speed: F.hoistFn_ identity
  }
  where
    validateMetric metric = F.Validation \form i ->
      if (F.getInput prx.metric form) == Just metric
        then (map (Just <<< Dollars)) <$> F.runValidation V.strIsInt form i
        else pure (pure Nothing)

