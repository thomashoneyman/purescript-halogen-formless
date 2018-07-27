module Example.Polyform.Types where

import Prelude

import Effect.Aff (Aff)
import Example.Polyform.Spec (Form, User)
import Formless as F

data Query a = HandleFormless (F.Message' Form User) a

type State = Unit

type ChildQuery = F.Query' Form User Aff
type ChildSlot = Unit
