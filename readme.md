# Formless

[![CI](https://github.com/thomashoneyman/purescript-halogen-formless/workflows/CI/badge.svg?branch=main)](https://github.com/thomashoneyman/purescript-halogen-formless/actions?query=workflow%3ACI+branch%3Amain)
[![Latest release](http://img.shields.io/github/release/thomashoneyman/purescript-halogen-formless.svg)](https://github.com/thomashoneyman/purescript-halogen-formless/releases)
[![Latest package set](https://img.shields.io/endpoint.svg?url=https://package-sets-badge-0lf69kxs4fbd.runkit.sh/halogen-formless)](https://github.com/purescript/package-sets)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

Formless is a flexible, extensible, type-safe Halogen component for building forms without boilerplate.

- [Examples & documentation site](https://thomashoneyman.github.io/purescript-halogen-formless/)
- [Source code for examples](https://github.com/thomashoneyman/purescript-halogen-formless/tree/main/example)

## Installation

Install Formless with Spago:

```sh
spago install halogen-formless
```

## Quick Start

You can write a basic Formless form in just a few lines of code. You are responsible for providing just a few pieces of information.

First, a form type that describes the fields in your form, along with their validation error type, user input type, and validated output type. Note: you can provide whatever custom error types you'd like, use `Void` to represent no possible errors, parse to whatever type you want, and none of your fields need to share any types.

Note: You will need to annotate your form newtype to tell the compiler that `r` is of type `Row Type -> Type`, and you need to derive a newtype instance for your form. You'll get compiler errors if you forget!

```purescript
import Prelude
import Data.Newtype (class Newtype, unwrap)

type Dog = { name :: String, age :: Age }

newtype Age = Age Int

derive instance newtypeAge :: Newtype Age _

instance showAge :: Show Age where
  show = show <<< unwrap

data AgeError = TooLow | TooHigh | InvalidInt

newtype DogForm (r :: Row Type -> Type) f = DogForm (r
  --          error    input  output
  ( name :: f Void     String String
  , age  :: f AgeError String Age
  ))

derive instance newtypeDogForm :: Newtype (DogForm r f) _
```

Next, the component input, which is made up of initial values and validation functions for each field in your form. Note: with your form type complete, the compiler will verify that your inputs are of the right type, that your validation takes the right input type, produces the right error type, and parses to the right output type, that fields exist at these proper keys, and more. There's no loss in type safety here! Plus, your validation functions can easily reference the value of other fields in the form, perform monadic effects, get debounced before running, and more.

You can generate sensible defaults for all input fields in your form by setting `initialInputs` to `Nothing`, or you can manually provide the starting value for each field in your form.

```purescript
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Formless as F

input :: forall m. Monad m => F.Input' DogForm m
input =
  { initialInputs: Nothing -- same as: Just (F.wrapInputFields { name: "", age: "" })
  , validators: DogForm
      { name: F.noValidation
      , age: F.hoistFnE_ \str -> case Int.fromString str of
          Nothing -> Left InvalidInt
          Just n
            | n < 0 -> Left TooLow
            | n > 30 -> Left TooHigh
            | otherwise -> Right (Age n)
      }
  }
```

Finally, the component spec, which is made up of a number of optional functions and types you can use to extend the Formless component. At minimum you will need to provide your own render function that describes how your form should be presented to the user. But you can also freely extend the Formless state, query, action, child slots, and message types, as well as provide your own handlers for your extended queries, actions, and child slots, and handle Formless messages internally without leaking information to a parent. You can extend Formless to an incredible degree -- or you can keep things simple and just provide render function. All extensions are optional.

For our small form, we'll do two things: we'll provide a render function, and when the form is submitted, we'll output a `Dog` to parent components. Along the way we'll wire things up so that input fields display their current value from form state; typing into an input field updates its value in state, also running the correct validation function; we'll display the validation error for `age` if there is one; and we'll wire up a submit button.

Note: If you would like to have your form raise no messages (rare), do not supply a `handleEvent` function. If you would like to raise the usual Formless messages (`Changed`, `Submitted`), then provide `H.raise` as your `handleEvent` function. If you would like to simply raise your form's validated output type (`Dog`, in this example), then provide `F.raiseResult` as your `handleEvent` function. Finally, if you want to do something else, you can write a custom function that does whatever you would like.

```purescript
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

spec :: forall input m. Monad m => F.Spec' DogForm Dog input m
spec = F.defaultSpec { render = render, handleEvent = F.raiseResult }
  where
  render st@{ form } =
    HH.form_
      [ HH.input
          [ HP.value $ F.getInput _name form
          , HE.onValueInput $ F.set _name
          ]
      , HH.input
          [ HP.value $ F.getInput _age form
          , HE.onValueInput $ F.setValidate _age
          ]
      , HH.text case F.getError _age form of
          Nothing -> ""
          Just InvalidInt -> "Age must be an integer"
          Just TooLow -> "Age cannot be negative"
          Just TooHigh -> "No dog has lived past 30 before"
      , HH.button
          [ HE.onClick \_ -> F.submit ]
          [ HH.text "Submit" ]
      ]
    where
    _name = Proxy :: Proxy "name"
    _age = Proxy :: Proxy "age"
```

Our form is now complete. It's easy to put this form in a parent page component:

```purescript
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.HTML as HH

data Action = HandleDogForm Dog

page :: forall q i o m. MonadAff m => H.Component q i o m
page = H.mkComponent
  { initialState: const unit
  , render: const render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  handleAction (HandleDogForm dog) = logShow (dog :: Dog)

  render = HH.slot F._formless unit (F.component (const input) spec) unit HandleDogForm
```

## Next Steps

Ready to move past this simple example? Check out the examples, which vary in their complexity:

- [Live examples / docs site](https://thomashoneyman.github.io/purescript-halogen-formless/)
- [Source code for examples](https://github.com/thomashoneyman/purescript-halogen-formless/tree/main/example)

### Running the examples locally

If you'd like to explore the example forms locally, you can run them by cloning this repository and then running these commands in the root of the project:

```sh
# Install PureScript dependencies
nix-shell

# Bundle the examples and open ./dist/index.html in your browser
spago -x example/example.dhall bundle-app --to ./dist/app.js
```

## Comments & Improvements

Have any comments about the library or any ideas to improve it for your use case? Please file an issue or reach out on the [PureScript user group](https://discourse.purescript.org).
