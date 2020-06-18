-- | This module exports helpers for working with Formless queries. Action-style
-- | queries are already specialized to `Unit` for you. Prefer these over using
-- | data constructors from the Formless query algebra. Remember that you can
-- | freely extend the Formless query algebra with your own queries by using the
-- | `injQuery` function.
module Formless.Query where

import Prelude

import Data.Functor.Variant as VF
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant)
import Formless.Types.Component (QueryF(..), Query, PublicAction)
import Formless.Types.Form (OutputField)
import Halogen as H
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery as CQ
import Prim.Row as Row

-- | Inject your own query into the Formless component. You will need to derive
-- | a `Functor` instance for your query type.
-- |
-- | ```purescript
-- | data MyQuery a = DoSomething a
-- | derive instance functorMyQuery :: Functor MyQuery
-- | ```
injQuery :: forall form q ps a. Functor q => q a -> Query form q ps a
injQuery = VF.inj (SProxy :: SProxy "userQuery")

-- | Convert a Formless public action to an action-style query. Any action from
-- | Formless.Action will work, but no others.
asQuery :: forall form q ps. Variant (PublicAction form) -> Query form q ps Unit
asQuery = VF.inj (SProxy :: SProxy "query") <<< H.mkTell <<< AsQuery

-- | Submit the form, returning the output of validation if successful
-- | and `Nothing` otherwise.
submitReply
  :: forall form query ps a
   . (Maybe (form Record OutputField) -> a)
  -> Query form query ps a
submitReply = VF.inj (SProxy :: SProxy "query") <<< SubmitReply

-- | When you have specified a child component within Formless and need to query it,
-- | you can do so in two ways.
-- |
-- | First, you can use `H.query` as usual within the `handleExtraQuery` function
-- | you provide to Formless as input. You can, for example, write your own query
-- | which manages the child component, write it into the render function you
-- | supply Formless, and then when that query is triggered, query the child
-- | component.
-- |
-- | Second, you can use the `sendQuery` function within your parent component's
-- | `handleAction` or `handleQuery` functions. Given the slot for Formless, the
-- | slot for the child component within Formless, and the query you'd like to
-- | send the child component, `sendQuery` will run through Formless and return
-- | the query result to the parent.
-- |
-- | For example, this is how you would query a dropdown component being run
-- | within a Formless form from the parent component.
-- |
-- | ```purescript
-- | -- in the parent component which mounts Formless
-- | handleAction = case _ of
-- |   SendDropdown (dropdownQuery -> do
-- |     result <- F.sendQuery _formless unit _dropdown 10 dropdownQuery
-- | ```
sendQuery
  :: forall outS outL inS inL form msg q ps cq cm pps r0 r1 st pmsg act m a
   . IsSymbol outL
  => IsSymbol inL
  => Row.Cons outL (Slot.Slot (Query form q ps) msg outS) r0 pps
  => Row.Cons inL (Slot.Slot cq cm inS) r1 ps
  => Ord outS
  => Ord inS
  => SProxy outL
  -> outS
  -> SProxy inL
  -> inS
  -> cq a
  -> H.HalogenM st act pps pmsg m (Maybe a)
sendQuery ol os il is cq =
  H.query ol os
    $ VF.inj (SProxy :: _ "query")
    $ SendQuery
    $ CQ.mkChildQueryBox
    $ CQ.ChildQuery (\k -> maybe (pure Nothing) k <<< Slot.lookup il is) cq identity
