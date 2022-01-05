# Formless

[![CI](https://github.com/thomashoneyman/purescript-halogen-formless/workflows/CI/badge.svg?branch=main)](https://github.com/thomashoneyman/purescript-halogen-formless/actions?query=workflow%3ACI+branch%3Amain)
[![Latest release](http://img.shields.io/github/release/thomashoneyman/purescript-halogen-formless.svg)](https://github.com/thomashoneyman/purescript-halogen-formless/releases)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-teal.svg)](http://github.com/thomashoneyman)

Formless helps you write forms in Halogen without the boilerplate.

- [Examples & documentation site](https://thomashoneyman.github.io/purescript-halogen-formless/)
- [Source code for examples](./example)
- [Migration of Real World Halogen from Formless 2 to Formless 3](https://github.com/thomashoneyman/purescript-halogen-realworld/pull/102)

⚠️ You are looking at the pre-release for Formless 3, which is not in the package sets. Please use the [v2.2.0](https://github.com/thomashoneyman/purescript-halogen-formless/tree/v2.2.0) tag if you are using Formless 2. ⚠️

## Installation

Install Formless with Spago:

```console
$ spago install halogen-formless
```

This version of Formless is not included in the official package set because it relies on a not-yet-merged pull request to `variant`. You will need to add these packages to your `packages.dhall` additions to support Formless 3:

```dhall
let upstream = ...

in  upstream
  with variant.version = "map-variant"
  with variant.repo = "https://github.com/MonoidMusician/purescript-variant"

  with halogen-formless = {
    repo = "https://github.com/thomashoneyman/purescript-halogen-formless",
    version = "main",
    dependencies =
      [ "convertable-options"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "foreign-object"
      , "halogen"
      , "heterogeneous"
      , "maybe"
      , "prelude"
      , "record"
      , "safe-coerce"
      , "type-equality"
      , "unsafe-coerce"
      , "unsafe-reference"
      , "variant"
      , "web-events"
      , "web-uievents"
      ]
  }
```

## Tutorial

We're going to write a form from scratch, demonstrating how to use Formless with no helper functions. This tutorial can serve as the basis for your real applications, but you'll typically write your own helper functions for common form controls and validation in your app. Make sure to check out the [examples directory](./example) after you read this tutorial to expand your knowledge!

Our form will let a user register their cat for pet insurance by recording its name, nickname, and age. Let's take the first step!

### Define a form type

We'll start by defining a type for our form.

```purs
type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( name     :: f String String String
  , nickname :: f String Void   (Maybe String)
  , age      :: f String String Int
  --              input  error  output
  )
```

Form types are typically defined as a row of form fields, where each form field specifies its input, error, and output type as arguments to `f`.

- The `input` type describes what the form field will receive from the user. For example, a text input will receive a `String`, while a radio group might use a custom sum type.
- The `error` type describes what validation errors can occur for this form field. We'll stick to `String` for our example, but you can create your own form- or app-specific error types.
- The `output` type describes what our input type will parse to, if it passes validation. For example, while we'll let the user type their cat's age into a text field and therefore accept a `String` as input, in our application we will only consider `Int` ages to be valid.

Take a moment and think about what the input, error, and output types for each of our three fields are. Our `nickname` field has an output type of `Maybe String` -- what do you think that represents?

Defining our form row this way provides maximum flexibility for defining other type synonyms in terms of the form row. This greatly reduces the amount of code you need to write for your form. For example, Formless requires that we provide an initial set of values for our form fields:

```purs
initialValues = { name: "", nickname: "", age: "" }
```

We can write a type for this value by writing a brand new record type, or by reusing our form type:

```hs
import Formless as F

-- Option 1: Define a new record type
type FormInputs = { name :: String, nickname :: String, age :: String }

-- Option 2: Reuse our form row
type FormInputs = { | Form F.FieldInput }
```

These two implementations of `FormInputs` are identical. However, reusing the form row requires less typing and ensures a single source of truth.

### Write component types

Formless is a higher-order component, which means that it takes a component as an argument and returns a new component. The returned component can have any input, query, output, and monad types you wish -- Formless is entirely transparent from the perspective of a parent component.

#### Public Types

Let's write concrete types for our component's public interface. We don't need any input or to handle any queries, but we'll have our form raise a valid `Cat` as its output.

```purs
-- Reusing our form row again! This type is identical to:
-- { name :: String, nickname :: Maybe String, age :: Int }
type Cat = { | Form F.FieldOutput }

type Query = Const Void

type Input = Unit

type Output = Cat

-- We now have the types necessary for our wrapped component,
-- which we'll run in `Aff`:
component :: H.Component Query Input Output Aff
```

#### Internal Types

Next, we'll turn to our internal component types: the state and action types (we don't need any child slots, so we'll hard code them to `()`).

Formless requires our component to support two actions:

- Your component must receive input of type `FormContext`, which includes the form fields and useful actions for controlling the form. It also includes any other input you want your component to take. By convention this action is called `Receive`.
- Your component must raise actions of type `FormlessAction` to Formless for evaluation. By convention this action is called `Eval`.

The `FormContext` and `FormlessAction` types you need to write for your `Action` type can be easily implemented by reusing your form row along with type synonyms provided by Formless. Let's define these two types for our form:

```purs
-- Our form will receive `FormContext` as input. We can specialize the Formless-
-- provided `F.FormContext` type to our form by giving it our form row applied
-- to the `F.FieldState` and `F.FieldAction` type synonyms.
--
-- The form context includes the current state of all fields in the form, so its
-- first argument is our form row applied to `F.FieldState`. It also includes a
-- set of actions for controlling the form, so our second argument is our form
-- row and component action type applied to `F.FieldAction`. Finally, the form
-- context passes through the input type we already defined for our component
-- (in our case, `Unit`), and so it takes the `Input` type as its third
-- argument. Finally, it provides some form-wide helper actions, and so we must
-- provide our `Action` type as the fourth argument.
type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action

-- Our form raises Formless actions for evaluation, most of which track the
-- state of a particular form field. We can specialize `F.FormlessAction` to our
-- form by giving it our form row applied to the `F.FieldState` type synonym.
type FormlessAction = F.FormlessAction (Form F.FieldState)
```

With our `FormContext` and `FormlessAction` types specialized, we can now implement our component's internal `Action` type:

```purs
data Action
  = Receive FormContext
  | Eval FormlessAction
```

The `FormContext` and `FormlessAction` types can be confusing the first time you see them. If they are a lot to take in, don't worry: you'll get used to them, and after you define them once you don't have to touch them again (any changs you make to your form will happen on the form row).

Our final component type is the `State` type. We don't need any extra state beyond what Formless gives us, so we'll just reuse the `FormContext` as our state type:

```purs
type State = FormContext
```

### Implement your form component

We can now write our form component and make use of the state and helper functions that Formless makes available to us.

You will typically implement your form component by applying Formless directly to `H.mkComponent`, which saves quite a bit of typing. The Formless higher-order component takes three arguments:

- A `FormConfig`, which lets you control some of Formless' behavior, like when validation should be run, and lets you lift Formless actions into your `Action` type. The only required option is `liftAction`; all other fields are entirely optional.
- A record of initial values for each field in your form. We already wrote an `initialValues` when we defined our form type, but since all our inputs are strings, we could also implement our initial form as a simple `mempty`. This is what's demonstrated below.
- Your form component, which must accept `FormContext` as input, handle queries of type `FormQuery`, and raise outputs of type `FormOutput`. Don't worry -- we'll talk more about each of these!

```purs
import Halogen as H
import Effect.Aff (Aff)
import Data.Maybe (Maybe(..))

form :: H.Component Query Input Output Aff
form = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: \context -> context
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
```

#### Rendering Your Form

The Formless form context provides you with the state of each field in your form, along with pre-made actions for handling change, blur, and other events. You can use this information to implement a basic form.

In the below example, we make use of a form-wide action (`handleSubmit`), field-specific actions (`handleChange`, `handleBlur`), and field-specific state (`value`, `result`).

```purs
form = F.formless ...
  where
  render :: FormContext -> H.ComponentHTML Action () Aff
  render { formActions, fields, actions } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit ]
      [ HH.div_
          [ HH.label_ [ HH.text "Name" ]
          [ HH.input
              [ HP.type_ HP.InputText
              , HP.placeholder "Scooby"
              , HP.value fields.name.value
              , HE.onValueInput actions.name.handleChange
              , HE.onBlur actions.name.handleBlur
              ]
            -- We can use the `result` field to check if we have an error
          , case fields.name.result of
              Just (Left error) -> HH.text error
              _ -> HH.text ""
          ]
      ]
```

It's tedious and error-prone manually wiring up form fields, so most applications should define their own reusable form controls by abstracting what you see here. You can see examples of that in the [examples directory](./example).

#### Handling Actions

Every form component you provide to Formless should implement a `handleAction` function that updates your component when new form context is provided and tells Formless to evaluate form actions when they arise in your component. A typical `handleAction` function in a form component looks like this:

```purs
form = F.formless ...
  where
  -- While our outer component's output type is `Output`, our inner component's
  -- output type is `F.FormOutput Output`, as we raise `FormlessAction`s to
  -- Formless for evaluation.
  handleAction
    :: Action
    -> H.HalogenM State Action () (F.FormOutput Output) Aff Unit
  handleAction = case _ of
    -- When we receive new form context, we need to update our form state so our
    -- component renders again.
    Receive context ->
      H.put context

    -- When a `FormlessAction` has been triggered we must raise it up to
    -- Formless for evaluation. We can do this with `F.eval`.
    Eval action ->
      F.eval action
```

You can freely add your own actions to your form for anything else your form needs to do. See the [examples](./example) for...examples!

#### Handling Queries

Formless uses queries to notify your form component of important events like when a form is submitted or reset, or when a form field needs to be validated.

Unlike previous versions of Formless, you don't provide any validation functions to the form directly. Instead, you will receive a `Validate` query that contains an input from your form. You are required to return an `Either error output` for that field back to Formless.

The most important benefit of this approach is that you can write validation functions that run in the context of your form component. That means that your validators can freely access your form state, including the state of other fields in the form, and you can evaluate actions in your component as part of validation (for example, making a request or setting the value of another field). We'll just explore pure validation in this example, but the [examples directory](./example) demonstrates various validation scenarios.

A typical `handleQuery` function uses the `handleSubmitValidate` or `handleSubmitValidateM` helper functions to only deal with form submission and validation events. In our case, we'll simply raise a successful form submission as output, and we'll provide a set of pure validation functions:

```purs
form = F.formless ...
  where
  handleQuery
    :: forall a. F.FormQuery _ _ _ _ a
    -> H.HalogenM State Action () (F.FormOutput Output) Aff (Maybe a)
  handleQuery = do
    let
      validateName :: String -> Either String String
      validateName input
        | input == "" = Left "Required"
        | otherwise = Right input

      validateNickname :: String -> Either Void (Maybe String)
      validateNickname input
        | input == "" = Right Nothing
        | otherwise = Right (Just input)

      validateAge :: String -> Either String Int
      validateAge input = case Int.fromString input of
        Nothing -> Left "Not a valid integer."
        Just n
          | n > 20 -> Left "No dog is over 20 years old!"
          | n <= 0 -> Left "No dog is less than 0 years old!"
          | otherwise -> Right n

      validation :: { | Form F.FieldValidation }
      validation =
        { name: validateName
        , nickname: validateNickname
        , age: validateAge
        }

      -- F.raise is a helper function for raising your own output types as
      -- component output directly.
      handleSuccess :: Cat -> H.HalogenM _ _ _ _ _ Unit
      handleSuccess = F.raise

    -- handleSubmitValidate lets you provide a success handler and a record
    -- of validation functions to handle submission and validation events.
    F.handleSubmitValidate handleSuccess F.validate validation
```

In a typical form, you wouldn't write out all these types, and your validation functions would probably live in a separate `Validation` module in your project. In the real world, a more typical `handleQuery` looks like this:

```purs
import MyApp.Validation as V

form = F.formless ...
  where
  handleQuery = F.handleSubmitValidate F.raise F.validate
    { name: V.required
    , nickname: V.optional
    , age: V.greaterThan 0 <=< V.lessThan 20 <=< V.int
    }
```

If you would like to see all possible events that your `handleQuery` function can handle, please see the implementation of `handleSubmitValidate`.

## Comments & Improvements

Have any comments about the library or any ideas to improve it for your use case? Please file an issue, or reach out on the [PureScript forum](https://discourse.purescript.org) or [PureScript chat](https://purescript.org/chat).
