# Formless

[![CircleCI](https://circleci.com/gh/thomashoneyman/purescript-halogen-formless/tree/master.svg?style=shield)](https://circleci.com/gh/thomashoneyman/purescript-halogen-formless/tree/master)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

Formless is a [renderless component](https://github.com/thomashoneyman/purescript-halogen-renderless) which helps you build forms in Halogen. Provide Formless with a form data type, validation function, submit function, and render function, and the component will handle the tedious parts of managing form state, errors, and submission.

- [Live examples / docs site](https://thomashoneyman.github.io/purescript-halogen-formless/)
- [Source code for examples](https://github.com/thomashoneyman/purescript-halogen-formless/tree/master/example)

### Installation

Install with Bower:

```sh
bower i --save purescript-halogen-formless
```

### Status

Formless is already used in production at [@citizennet](https://github.com/citizennet) and is going through final updates for a v1 release. Do you have any comments about the library or any ideas to improve it for your use case? Please file an issue, send me an email, or reach out on the [PureScript user group](https://purescript-users.ml).


# Overview

The default approach to forms in Halogen is to write a component and, for every field in your form, queries to handle changes on those fields and validation. Each form field lives in the state type along with validation results, summary information (like whether fields have been edited), and a possible form output.

Formless helps abstract away most of the messy details of managing form state without imposing any restrictions on how you render your form.

To demonstrate, let's build a signup form in Formless.

## Data Types

We'll start with the data type we want our form to result in: a `User`.

```purescript
type User =
  { id :: Int
  , name :: String
  }
```

This is the data type we'll use throughout our application, but our form will have different fields altogether: we want them to provide two passwords we'll send to the server, and we don't have an ID for them until the form has been submitted.

Formless requires a specific shape from your `Form` data type. You are expected to write a newtype that takes an argument, `f`. This argument will be one of many types Formless uses internally to manage your form. `f` itself expects three type arguments:

- an `error` type, which represents possible validation errors for the field
- an `input` type, which represents the value the user will provide when interacting with the field
- an `output` type, which represents the type you'd like to result from successful validation

Here's what our form type looks like:

```purescript
-- We'll assume you've defined `ValidationError` elsewhere
type Errors = Array ValidationError

newtype Form f = Form
  { name      :: f Errors String String
  , password1 :: f Errors String Encrypted
  , password2 :: f Errors String Encrypted
  }
derive instance newtypeForm :: Newtype (Form f) _
```

## Component Inputs

Now that we have a form type and an output type we can produce the various inputs that the Formless component requires:

```purescript
import Formless as F

type FormlessInput m =
  { formSpec :: Form F.FormSpec
  , validator :: Form F.InputField -> m (Form F.InputField)
  , submitter :: Form F.OutputField -> m User
  , render :: F.State Form User m -> F.HTML' Form User m
  }
```

### Form Spec

The first thing Formless requires is a spec, which is simply a list of the fields in your form with their initial values and has the type `Form F.FormSpec`. Remember: `Form` is our custom newtype we defined a moment ago, and it was awaiting a type that would be applied to the error, input, and output types we defined for each field -- like `F.FormSpec`!

```purescript
newtype FormSpec error input output = FormSpec input
```

Applied to our form, a `FormSpec` represents the input type only. We can give Formless a valid form spec by just supplying concrete input values for each field:

```purescript
formSpec :: Form F.FormSpec
formSpec = Form
  { name: FormSpec ""
  , password1: FormSpec ""
  , password2: FormSpec ""
  }
```

It's a little tedious writing out all those newtypes, so `Formless.Spec.Transform` provides helper functions to generate them for you:

```purescript
formSpec :: Form F.FormSpec
formSpec = mkFormSpec
  { name: ""
  , password1: ""
  , password2: ""
  }
```

In fact, you don't even have to do this: if your input types belong to the `Formless.Initial` type class (all monoidal values do), it can generate the values for you from a row:

```purescript
formSpec :: Form F.FormSpec
formSpec = mkFormSpecFromRow $
  RProxy :: RProxy ( name :: String, password1 :: String, password2 :: String )
```

### Validator

The next thing Formless requires is a validator: a function that will be run on the form to validate the inputs and produce the specified output types. Formless only expects a function from `∀ m. Monad m => Form F.InputField -> m (Form F.InputField)`, so you can provide whatever validation you would like, including validation that involves side effects like interacting with a server.

```purescript
newtype InputField error input output = InputField
  { -- The type captured by your HTML events
    input :: input
    -- Whether the user has interacted with the field yet
  , touched :: Boolean
    -- The result of validation: Nothing if not yet validated,
    -- Just (Left error) if validation failed, and Just (Right output)
    -- if validation succeeded.
  , result :: Maybe (Either error output)
  }
```

Your validation function has access to the `input`, `touched`, and `result` fields above, but it can be a little tedious validating an input and then setting its result, especially if you only want to validate `touched` fields. Most Formless users rely on [purescript-validation](https://github.com/purescript/purescript-validation) or [purescript-polyform](https://github.com/paluh/purescript-polyform), and Formless provides helper functions for these libraries.

Let's see an example of a field validation function written with Polyform.

```purescript
import Polyform.Validation (Validation(..), hoistFnV)
import MyModule.Validators as V

-- An example Polyform validator
validateEncrypt :: ∀ m. Monad m => Validation m V.Errors String Encrypted
validateEncrypt = hoistFnV \str ->
  if null str
    then Invalid [ V.EncryptionFailed ]
    else pure (Encrypted str)
```

We can provide a field validation function like this to every field that will be in our form, and then use the `applyOnInputFields` helper function from Formless to convert these functions to run on the `input` field and store their result in the `result` field:

```
import Formless.Validation.Polyform (applyOnInputFields)

-- Each validator in the record should have the type `Validation m error input output`
validator :: ∀ m. MonadEffect m => Form F.InputField -> m (Form F.InputField)
validator = applyOnInputFields
  { name: V.nonEmpty
  , password1: V.minLength 8 *> validateEncrypt
  , password2: V.minLength 8 *> validateEncrypt
  }
```

Note: if you get a type error about being unable to match a constrained type, then help the compiler out by applying `identity` to your record of validators before providing it to `applyOnInputFields`:

```purescript
validator = applyOnInputFields (identity { name: V.nonEmpty, ... })
```

### Submitter

Formless manages validation and failed submit attempts on your behalf, only notifying you with a message when your expected result type has been successfully created. To do that, it accepts a `submitter :: ∀ m out. Monad m => Form F.OutputField -> m out` function.

```purescript
-- A type representing only the successful parsed values in your Form type
newtype OutputField error input output = OutputField output

-- `unwrapOutputs` is a helper function that will unwrap all these newtypes on your
-- behalf. Used on our custom Form type, it'd apply this transformation:
unwrapOutputs' :: Form F.OutputField -> { name :: String, password1 :: Encrypted, password2 :: Encrypted }
unwrapOutputs' = F.unwrapOutputs
```

The function allows you to take a fully-valid form and perform some transformations and side effects with it before returning your output type to you in a message. As an example, let's send our signup form to the server and retrieve our new user id:

```purescript
submitter :: ∀ m. MonadEffect m => GroupForm F.OutputField -> m User
submitter form = do
  -- We'll pretend to hit the server
  userId <- randomInt
  -- We'll delete our unused fields and insert the new user ID
  let user =
        Record.delete (SProxy :: SProxy "password1")
        $ Record.delete (SProxy :: SProxy "password2")
        $ Record.insert (SProxy :: Sproxy "id") userId
        $ F.unwrapOutput form
  pure user
```

### Render Function

The last thing you're expected to provide is a render function. Formless is a renderless component, so it provides no rendering at all and expects you to provide an entire render function of the type `∀ m. F.State Form User m -> F.HTML' Form User m`. To learn more about renderless components, see the [purescript-halogen-renderless](https://github.com/thomashoneyman/purescript-halogen-renderless) library.

The main things to keep in mind when writing a render function for Formless:

- You can pass arguments to the function before it is given to Formless (like your parent state). When the parent component re-renders, these values will be given to Formless anew.
- You can extend Formless' functionality by embedding your own queries in the render function with `Raise`
- You can mount external components inside Formless and control them from the parent with `Send`
- You should use `F.Modify` and `F.Validate` on change and blur events in your HTML

Let's write a simple render function for our signup form. We'll use `getInput` to retrieve the input value of a field with a given symbol; `setInput` to overwrite the input value with a new one on `onValueInput` events; and a lens into a potential `error` field in the lens to show an error, if there is one, for each input.

```purescript
renderFormless :: ∀ m. F.State Form User m -> F.HTML' Form User m
renderFormless fstate =
  HH.div_
  [ HH.input
    [ HP.value $ F.getInput _name fstate.form
    , HE.onBlur $ HE.input_ F.Validate
    , HE.onValueInput $ HE.input $ F.Modify <<< F.setInput _name
    ]
  , HH.text $ showError $ Lens.preview (F._Error _name) fstate.form
  , HH.input
    [ HP.value $ F.getInput _password1 fstate.form
    , HE.onBlur $ HE.input_ F.Validate
    , HE.onValueInput $ HE.input $ F.Modify <<< F.setInput _password1
    ]
  , HH.text $ showError $ Lens.preview (F._Error _name) fstate.form
  , HH.input
    [ HP.value $ F.getInput _password1 fstate.form
    , HE.onBlur $ HE.input_ F.Validate
    , HE.onValueInput $ HE.input $ F.Modify <<< F.setInput _password2
    ]
  , HH.text $ showError $ Lens.preview (F._Error _name) fstate.form
  ]

  where

  showError :: Maybe Errors -> String
  showError Nothing = ""
  showError (Just arr) = fromMaybe "" $ show $ head arr

  _name = SProxy :: SProxy "name"
  _password1 = SProxy :: SProxy "password1"
  _password2 = SProxy :: SProxy "password2"
```

## Mounting The Component

Whew! With those four functions, the `Form` type, and the `User` type, we've now got everything necessary to run Formless. Let's bring it all together by mounting the component and handling its `Submitted` output message:

```purescript
import Formless as F

data Query a
  = Formless (F.Message' Form User) a

type ChildQuery = F.Query' Form User Aff
type ChildSlot = Unit

component :: H.Component HH.HTML Query Unit Void Aff
component = H.parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }

  where

  render :: Unit -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render st =
    HH.div_
    [ HH.h1 "My Form"
    , HH.slot
        unit
        F.component
        { formSpec, validator, submitter, render: renderFormless }
        ( HE.input Formless )
    ]

  eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void Aff
  eval (Formless m a) = case m of
    F.Submitted user -> do
      Console.log $ "Got a user! " <> show (user :: User)
    _ -> pure a
```

# Next Steps

Formless is already used in production at [@citizennet](https://github.com/citizennet) and is going through final updates for a v1 release. Do you have any comments about the library or any ideas to improve it for your use case? Please file an issue or reach out on the [PureScript user group](https://purescript-users.ml).

Ready to move past this simple example? Check out the examples, which vary in their complexity:

- [Live examples / docs site](https://thomashoneyman.github.io/purescript-halogen-formless/)
- [Source code for examples](https://github.com/thomashoneyman/purescript-halogen-formless/tree/master/example)

If you're curious to learn more about how to use renderless components effectively, or build your own:

- [purescript-halogen-renderless](https://github.com/thomashoneyman/purescript-halogen-renderless)

There are other renderless components that work well with Formless:

- [purescript-halogen-select: typeaheads, dropdowns, and more](https://github.com/citizennet/purescript-halogen-select)
