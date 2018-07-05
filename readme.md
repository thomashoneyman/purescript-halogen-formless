# Formless

Formless is a renderless component which helps you build forms in Halogen.

Forms can be designed as data, but eventually you'll need to render an interactive form to the DOM. This is tedious and boilerplate-heavy in Halogen. Once you have a form spec, you can use Formless to handle all the wiring for validation, dirty states, field updates, and so on.

# Use

The component implemented in this project is a working renderless component. Either take it and use it as a scaffold for a new component you're building and would like to share, or use it to update an existing component to be renderless.

For demonstration purposes this is a fully-functioning Halogen app.

- `yarn` will install everything you need
- `yarn build` will build the library, but not the app
- `yarn build-all` will build the library and application, which you can view with the included `index.html` file.

## Other Notes

This library depends on `purescript-halogen-renderless` for helper functions and the original template for the renderless component. If you would like to learn more about how to build these components, or you're ready to build your own, see the library:

- [purescript-halogen-renderless](https://github.com/thomashoneyman/purescript-halogen-renderless)
