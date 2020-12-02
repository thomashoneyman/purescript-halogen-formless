# Formless

[![CI](https://github.com/thomashoneyman/purescript-halogen-formless/workflows/CI/badge.svg?branch=main)](https://github.com/thomashoneyman/purescript-halogen-formless/actions?query=workflow%3ACI+branch%3Amain)
[![Latest release](http://img.shields.io/github/release/thomashoneyman/purescript-halogen-formless.svg)](https://github.com/thomashoneyman/purescript-halogen-formless/releases)
[![Latest package set](https://img.shields.io/endpoint.svg?url=https://package-sets-badge-0lf69kxs4fbd.runkit.sh/halogen-formless)](https://github.com/purescript/package-sets)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

Formless helps you build pain-free forms using Halogen Hooks. This Hook provides essential building blocks for building form utilities specific to the needs of your application, and otherwise tries to stay out of your way.

- [Examples & documentation site](https://thomashoneyman.github.io/purescript-halogen-formless/)
- [Source code for examples](https://github.com/thomashoneyman/purescript-halogen-formless/tree/main/example)

## Installation

Install Formless with Spago:

```sh
spago install halogen-formless
```

## Quick Start

You can write a basic Formless form in just a few lines of code. In Formless, a _form_ is made up of a set of _fields_. You are responsible for providing the set of fields in your form, and Formless will build a form from them and manage your fields on your behalf. Formless doesn't come with any fields out of the box: you'll need to write your own for your application.

Let's walk through implementing a simple contact form. Once implemented, we'll extract relevant parts of the form into reusable helpers we can use throughout our application. This short demonstration is just one way to organize your own forms code, but Formless is flexible: feel free to use your own patterns!

### A Simple Contact Form

Here's a simple contact form implemented with Formless:

```purs
contactForm :: forall q i o m. MonadEffect m => H.Component q i o m
contactForm = Hooks.component \_ _ -> Hooks.do
  -- [1]: Create the form with `useForm`, `initialFormState`, and `buildForm`.
  form <- Formless.useForm (\_ -> Formless.initialFormState) $ Formless.buildForm
  -- [2]: Provide `buildForm` with a set of `FormField` to build.
    { firstName: Formless.FormField (Proxy2 :: _ m) \field -> Hooks.do
  -- [3]: Implement the form field using the provided `FormFieldInput`.
        let
          currentValue = fromMaybe "" field.value
          validated = NonEmptyString.fromString currentValue
          input =
            HH.input
              [ HE.onValueInput (Just <<< field.onChange)
              , HP.value currentValue
              ]

  -- [4]: Return at least a `value`, and also any additional fields you want to.
  --      In this case, we are also returning an `input` field which contains
  --      the rendered HTML for the form field. Note that our `value` here is
  --      not a string, but rather a `NonEmptyString` -- we've parsed an output
  --      value for the field which is not the same as its input value.
        Hooks.pure { value: validated, input }

    , lastName: Formless.FormField (Proxy2 :: _ m) \field -> Hooks.do
        let
          currentValue = fromMaybe "" field.value
          validated = NonEmptyString.fromString currentValue
          input =
            HH.input
              [ HE.onValueInput (Just <<< field.onChange)
              , HP.value currentValue
              ]

        Hooks.pure { value: validated, input }

    , message: Formless.FormField (Proxy2 :: _ m) \field -> Hooks.do
        let
          input =
            HH.textarea
              [ HE.onValueInput (Just <<< field.onChange)
              , HP.value (fromMaybe "" field.value)
              ]

        Hooks.pure { value: field.value, input }
    }

  Hooks.pure $
    HH.form
      [ HE.onSubmit (Just <<< liftEffect <<< Web.Event.preventDefault) ]
  -- [5]: Render the form fields returned by Formless. You can use the field
  --      value and any other record fields you included (in our case, the extra
  --      `input` field).
      [ form.fields.name.input
      , form.fields.message.input
      , HH.button
  -- [6]: Use other form metadata, like whether the whole form is valid or
  --      whether the form has been touched to implement your render code. Here,
  --      we only enable submission if the form is complete.
          [ HP.disabled (isNothing form.value || not form.touched)
          , HP.type_ HH.ButtonSubmit
          ]
          [ HH.text "Submit" ]
      ]
```

### Extracting Common `FormField`

Forms are made up of `FormField`, where a `FormField` is comprised of a proxy for `m` (necessary for type inference) and a function from `FormFieldInput` to a Hook which produces at least an output value. However, the Hook that you write can contain as many additional fields as ou would like -- in our case, we included a rendered output control.

Form fields are the best place to start extracting shared utilities for your application. For example, our `firstName` and `lastName` fields are essentially identical! Let's extract them into shared `FormField`:

```purs
requiredText
  :: forall w m
   . Proxy2 m
  -> FormField m Hooks.Pure (input :: HH.HTML w (HookM m Unit)) String NonEmptyString
requiredText proxy = FormField proxy \field -> Hooks.do
  let
    currentValue = fromMaybe "" field.value
    validated = NES.fromString currentValue
    input = HH.input [ HE.onValueInput (Just <<< field.onChange), HP.value currentValue ]

  Hooks.pure { value: validated, input }
```

We can now replace `firstName` and `lastName` with our new implementation. For example, here's the diff for replacing `firstName`:

```diff
+{ firstName: requiredText (Proxy2 :: _ m)
-{ firstName: FormField (Proxy2 :: _ m) \field -> Hooks.do
-    let
-      currentValue = fromMaybe "" field.value
-      validated = NonEmptyString.fromString currentValue
-      input =
-        HH.input
-          [ HE.onValueInput (Just <<< field.onChange)
-          , HP.value currentValue
-          ]
-
-    Hooks.pure { value: validated, input }
```

We've extracted a reusable `FormField`! This is among the simplest of form fields, but a typical application might reuse much richer form fields like sets of checkboxes, fields with asynchronous validation, fields which provide their own API so they can be focused, cleared, or have their values set imperatively, fields which wrap third-party Halogen components, and more. You can see more of these fields in the examples directory.

Now let's briefly look at the `FormFieldInput` you can use to implement your `FormField`:

```purs
type FormFieldInput m i =
  { onChange :: i -> HookM m Unit
  , value :: Maybe i
  , reset :: HookM m Unit
  }
```

You're provided three things:

1. An `onChange` handler, which should be used any time your form field's value changes (for example, when the user types into a text field).
2. A `value`, which represents the form state for this field, where `Nothing` means that this field is untouched and `Just i` means the field currently contains a value of type `i`.
3. A `reset` function, which can be used to reset the field back to an empty value.

Armed with these helpers, let's take a look at the `FormField` type:

```purs
data FormField m h ro i o = FormField (Proxy2 m) (FormFieldInput m i -> Hooks.Hook m h { value :: Maybe o | ro })
```

A `FormField` is essentially a function which accepts `FormFieldInput` and returns a Hook, where the Hook returns a record containing at least a `value` field with the output of this form field. You also must provide a proxy for the `m` monad type used in your form, which ensures the compiler can infer the correct type for your form.

This is a bit of a confusing type, so lets put it side-by-side with our reusable text field:

```purs
-- the FormField type definition from Formless
data     FormField m h          ro                                  i      o

-- an example form field implemented in your application
field :: FormField m Hooks.Pure (input :: HH.HTML w (HookM m Unit)) String NonEmptyString
```

Let's break it down.

- `m` represents the monad used in the form field. Our form field is compatible with any monad, so the type parameter is left open.
- `h` represents any Hooks we used to implement our form field. Our field simply calls `Hooks.pure`, so we use `Hooks.Pure`. However, you can freely use any Hooks you'd like in the body of your field.
- `ro` represents the row of output fields your form field produces in addition to the required `value :: Maybe o` field. In our case, we're also producing an `input` field which contains rendered HTML.
- `i` represents the input type stored in our form state. Our form field uses `String`.
- `o` represents the output type produced by our form field. This is usually the result of validation; in our case, we validate that the provided string is not empty and produce a `NonEmptyString` as our output. A form is only considered valid when all of its fields have produced some output value.

Please see the examples for a larger set of form fields you might wish to use as building blocks in your application.

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
