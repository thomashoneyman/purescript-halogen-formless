# Formless

[![CircleCI](https://circleci.com/gh/thomashoneyman/purescript-halogen-formless/tree/master.svg?style=shield)](https://circleci.com/gh/thomashoneyman/purescript-halogen-formless/tree/master)
[![Latest release](http://img.shields.io/github/release/thomashoneyman/purescript-halogen-formless.svg)](https://github.com/thomashoneyman/purescript-halogen-formless/releases)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

Formless is a [renderless component](https://github.com/thomashoneyman/purescript-halogen-renderless) which helps you build forms in Halogen. Provide Formless with some initial inputs, validators, a submit function, and a render function, and the component will handle the tedious parts of managing form state, errors, submission, and more.

You can write a complete Halogen form component with multiple fields, validation, parsing, and errors in less than 100 lines of code (only ~20 lines of which are from Formless).

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
  , email :: Email
  }
```

This is the data type we'll use throughout our application, but our form will have different fields altogether: we want them to provide two passwords we'll send to the server, and we don't have an ID for them until the form has been submitted.

Formless requires a specific shape from your `Form` data type. You are expected to write a newtype that takes two arguments, `r` and `f` below, and a row containing the fields in your form.

The first argument is `(# Type -> Type)` and turns a row of types into a concrete type. For example, you can fill in `Record` to get a record; `Record (name :: String)` is the same as `{ name :: String }`.

The second argument is `(Type -> Type -> Type -> Type)` and will be filled in with one of many types Formless uses internally to manage your form. The three type arguments that `f` expects are:

- an `error` type, which represents possible validation errors for the field
- an `input` type, which represents the value the user will provide when interacting with the field
- an `output` type, which represents the type you'd like to result from successful validation

You don't need to manage or worry about these two arguments much; they're mostly filled in by Formless on your behalf. Your biggest focus will be on defining the fields in your form with their input, error, and output types. 

Here's what our form type looks like:

```purescript
newtype Form r f = Form (r
  ( name      :: f ValidationError String String
  , password1 :: f ValidationError String Encrypted
  , password2 :: f ValidationError String Encrypted
  , email     :: f ValidationError String Email
  ))
derive instance newtypeForm :: Newtype (Form f r) _
```

<details>
  <summary>Expand to see the definition of <code>ValidationError</code>, <code>Encrypted</code>, and <code>Email</code> types</summary>

```purescript
newtype Encrypted = Encrypted String
newtype Email = Email String

data ValidationError
  = Required
  | NotEqual String String
  | TooShort Int
  | EncryptionFailed
  | EmailIsUsed
  | EmailInvalid
```

</details>

## Component Inputs

Now that we have a form type and an output type we can produce the `Input` type that the Formless component requires. While we'll take a closer look at each of these types in the next few sections, here's a quick primer on what these types are:

- `inputs`: Your `Form` newtype around a record, where each field contains its initial, starting value
- `validators`: Your `Form` newtype around a record, where each field contains a validation function which will process its input value
- `submitter`: A function that accepts as an argument your `Form` newtype around a record, where each label is from your `Form` row and each field is an `OutputField` containing the output of successful validation, and produces the output value of your choice (in our case, a `User`).
- `render`: The render function the component will use, which is the standard `State -> HTML` type in Halogen


```purescript
import Formless as F

type FormlessInput m =
  { inputs :: Form Record F.InputFields
  , validators :: Form Record (F.Validation Form m)
  , submitter :: Form Record F.OutputField -> m User
  , render :: F.State Form User m -> F.HTML' Form User m
  }
```

### Form Inputs

The first thing Formless requires is a record of the fields in your form with their initial values. It has the type `Form Record F.InputField`. Remember: `Form` is our custom newtype we defined a moment ago, and it was awaiting a type that would be applied to the error, input, and output types we defined for each field -- like `F.InputField`!

```purescript
newtype InputField error input output = InputField input
```

Applied to our form, an `InputField` represents the input type only. We can give Formless a valid record of inputs by just supplying concrete input values for each field:

```purescript
inputs :: Form Record F.InputField
inputs = Form
  { name: InputField ""
  , password1: InputField ""
  , password2: InputField ""
  , email: InputField ""
  }
```

It's a little tedious writing out all those newtypes, so `Formless.Spec.Transform` provides helper functions to generate them for you:

```purescript
inputs :: Form Record F.InputFields
inputs = F.wrapInputFields
  { name: ""
  , password1: ""
  , password2: ""
  , email: ""
  }
```

In fact, you don't even have to do this: if your input types belong to the `Formless.Initial` type class (all monoidal values do), it can generate the values for you from a proxy for your form:

```purescript
proxy :: F.FormProxy Form
proxy = F.FormProxy

inputs :: Form Record F.InputField
inputs = F.mkInputFields proxy
```

### Validation

The next thing Formless requires is a record of validators: functions that will be run on the form to validate the inputs and produce the specified output types. Every field in this record ought to use the Formless `Validation` type:

```purescript
newtype Validation form m error input output
  = Validation (form Record FormField -> input -> m (Either error output))
```

This type represents a function which takes your entire form, the input for this particular field, and produces either an error or result.

- This function can be monadic, so you can do things like confirm with a server that an email is not already in use.
- This function takes your entire form as an argument, so you can use the values of other fields during validation. For example, you could verify that two password fields are equal to one another.
- If you are using `purescript-validation` and already have a composed validation function that results in `V`, then you can convert it into a Formless validator with `hoistFnE_ <<< Data.Validation.Semigroup.toEither` (or the `Semiring` module).

The `FormField` newtype represents the state of every field in the form:

```
newtype FormField e i o = FormField
  { -- The value the user will input
    input :: i
    -- Whether the field has been modified yet (validators ignore untouched fields)
  , touched :: Boolean
    -- The result of validation, IF validation has been run on this field
  , result :: Maybe (Either e o)
  }
```

Let's see some examples of validators written in this style:

```purescript

-- This helper function lets you take any function from `input` to `output` and turns it into
-- the Validation type from Formless.
hoistFn_ :: ∀ form m e i o. Monad m => (i -> o) -> Validation form m e i o
hoistFn_ f = Validation $ const $ pure <<< pure <<< f

-- For example, this validator simply transforms the input `Int` into a `String` using `hoistFn_`
-- output.
myStringValidator :: ∀ form m. Monad m => Validation form m Void Int String
myStringValidator = hoistFn_ show

-- This helper function lets you take any function from `input` to `Either error output` and turns
-- it into the Validation type from Formless.
hoistFnE_ :: ∀ form m e i o. Monad m => (i -> Either e o) -> Validation form m e i o
hoistFnE_ f = Validation $ const $ pure <<< f

-- For example, this validator makes sure that the string is not empty
isNonEmpty :: ∀ form m. Monad m => Validation form m ValidationError String String
isNonEmpty = hoistFnE_ $ \str ->
  if null str
     then Left Required
     else Right str

-- This validator transforms the input into an `Email` type if successful.
validEmail :: ∀ form m. Monad m => Validation form m ValidationError String Email
validEmail = hoistFnE_ $ \str ->
  if contains (Pattern "@") str
     then Right (Email str)
     else Left EmailInvalid

-- Continuing the trend, this helper takes a function from `input` to a monad `m (Either error output)` and
-- turns it into the Validation type from Formless.
hoistFnME_ :: ∀ form m e i o. Monad m => (i -> m (Either e o)) -> Validation form m e i o
hoistFnME_ f = Validation $ const f

-- For example, this validator makes sure that an email address is not in use. Notice how it relies
-- on the input value already being an `Email` -- we'll see how to chain validators together so this
-- can be used with `validEmail` in a moment.
emailNotUsed :: ∀ form m. Validation form Aff ValidationError Email Email
emailNotUsed = hoistFnME $ \email -> do
  isUsed <- checkEmailIsUsed :: Email -> Aff Boolean
  pure $
    if isUsed
      then Right email
      else Left EmailIsUsed

-- Now, let's do something a little more complex. Let's validate that two passwords are equal to one another.

-- This time, we want to rely on our existing `Form` as an argument for our validation, so instead of using
-- `hoistFnE_` we'll reach for `hoistFn`, which doen't throw away the form argument.
-- it into the Validation type from Formless.
hoistFnE :: ∀ form m e i o. Monad m => (form Record FormField -> i -> Either e o) -> Validation form m e i o
hoistFnE f = Validation $ \form i -> pure $ f form i

-- We'll use `getInput` from Formless to retrieve the input value of the field "password1" from the form, and then
-- we'll validate that the current field is equal to it. Formless can prove that a "password1" field exists using
-- your form row, so you'll never access a value you don't have.
equalsPassword1 :: ∀ m. Monad m => Validation Form m ValidationError String String
equalsPassword1 = hoistFnE $ \form str ->
  let p1 = F.getInput (SProxy :: SProxy "password1") form
   in if str == p1
        then Right str
        else Left $ NotEqual str p1
```

These validators are building blocks that you can compose together to validate any particular field. Now that we've got some validation functions we can provide our `validators` record to Formless:

```purescript
validator :: Form Record (F.Validation Form Aff)
validator = Form
  { name: isNonEmpty
  , password1: isNonEmpty >>> hoistFn_ Encrypted
  , password2: isNonEmpty >>> equalsPassword1 >>> hoistFn_ Encrypted
  , email: validEmail >>> emailIsUsed
  }
```

Note how validators can be composed: `validEmail` takes a `String` and produces an `Email`, which is then passed to `emailIsUsed`, which takes an `Email` and produces an `Email`. You can use this to build up validators that change a field's output type over time. You can also use `hoistFn` to transform the output type at the end of validation, like the two password fields.


### Submitter

Formless manages validation and failed submit attempts on your behalf, only notifying you with a message when your expected result type has been successfully created. To do that, it accepts a `submitter :: ∀ m out. Monad m => Form Record F.OutputField -> m out` function.

- `OutputField` represents only the output type for a field, like `Email` or `Encrypted`
- Since `submitter` is monadic, you can perform effects like sending your data to the server to fetch an ID

```purescript
-- A type representing only the successful parsed values in your Form type
newtype OutputField error input output = OutputField output

-- `unwrapOutputFields` is a helper function that will unwrap all these newtypes on your behalf.
-- Used on our custom Form type, it'd apply this transformation:
unwrapOutput' :: Form Record F.OutputField -> { name :: String, password1 :: Encrypted, password2 :: Encrypted, email :: Email }
unwrapOutput' = F.unwrapOutputFields
```

The function allows you to take a fully-valid form and perform some transformations and side effects with it before returning your output type to you in a message. As an example, let's send our signup form to the server and retrieve our new user id:

```purescript
submitter :: ∀ m. MonadEffect m => Form Record F.OutputField -> m User
submitter form = do
  -- We'll pretend to hit the server
  userId <- liftEffect $ randomInt 0 10
  -- We'll delete our unused fields and insert the new user ID
  let user =
        form
        # F.unwrapOutputFields
        # Record.delete (SProxy :: SProxy "password1")
        # Record.delete (SProxy :: SProxy "password2")
        # Record.insert (SProxy :: SProxy "id") userId
  pure user
```

### Render Function

The last thing you're expected to provide is a render function. Formless is a renderless component, so it provides no rendering at all and expects you to provide an entire render function of the type `∀ m. F.State Form User m -> F.HTML' Form User m`. To learn more about renderless components, see the [purescript-halogen-renderless](https://github.com/thomashoneyman/purescript-halogen-renderless) library.

The main things to keep in mind when writing a render function for Formless:

- You can pass arguments to the function before it is given to Formless (like your parent state). When the parent component re-renders, these values will be given to Formless anew.
- You can extend Formless' functionality by embedding your own queries in the render function with `Raise`
- You can mount external components inside Formless and control them from the parent with `Send`
- You should use `F.modify` to modify a field on change events, `F.validate` to validate fields, and `F.modifyValidate` to do both
- There are functions to get various parts of a field, given a symbol; these include `getInput`, `getResult`, `getError`, and more.

Let's write a render function using `modifyValidate` and `getInput`, using symbol proxies we've defined in the `where` clause:

```purescript
renderFormless :: ∀ m. F.State Form User m -> F.HTML' Form User m
renderFormless fstate =
  HH.div_
  [ HH.input
    [ HP.value $ F.getInput _name fstate.form
    , HE.onValueInput $ HE.input $ F.modifyValidate _name
    ]
  , HH.input
    [ HP.value $ F.getInput _password1 fstate.form
    , HE.onValueInput $ HE.input $ F.modifyValidate _password1
    ]
  , HH.input
    [ HP.value $ F.getInput _password2 fstate.form
    , HE.onValueInput $ HE.input $ F.modifyValidate _password2
    ]
  , HH.input
    [ HP.value $ F.getInput _email fstate.form
    , HE.onValueInput $ HE.input $ F.modifyValidate _email
    ]
  ]

  where

  _name = SProxy :: SProxy "name"
  _password1 = SProxy :: SProxy "password1"
  _password2 = SProxy :: SProxy "password2"
  _email = SProxy :: SProxy "email"
```

It can be tedious to write out symbol proxies for every field you want to access in a form. You can instead generate a record of these proxies automatically using the `mkSProxies` function:

```
prx :: F.SProxies Form
prx = F.mkSProxies (F.FormProxy :: F.FormProxy Form)

-- These are now equivalent
x = SProxy :: SProxy "name"
x = prx.name

```

Now, instead of writing out proxies over and over, you can just import the proxies record!


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
        { inputs, validators, submitter, render: renderFormless }
        ( HE.input Formless )
    ]

  eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void Aff
  eval (Formless m a) = case m of
    F.Submitted user -> a <$ do
      liftEffect $ Console.log $ "Got a user! " <> show (user :: User)
    _ -> pure a
```

# Next Steps

Ready to move past this simple example? Check out the examples, which vary in their complexity:

- [Live examples / docs site](https://thomashoneyman.github.io/purescript-halogen-formless/)
- [Source code for examples](https://github.com/thomashoneyman/purescript-halogen-formless/tree/master/example)

If you're curious to learn more about how to use renderless components effectively, or build your own:

- [purescript-halogen-renderless](https://github.com/thomashoneyman/purescript-halogen-renderless)

There are other renderless components which work well with Formless:

- [purescript-halogen-select: typeaheads, dropdowns, and more](https://github.com/citizennet/purescript-halogen-select)

