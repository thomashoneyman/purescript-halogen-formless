# Formless

Formless is a renderless component which helps you build forms in Halogen.

Forms can be designed as data, but eventually you'll need to render an interactive form to the DOM. This is tedious and boilerplate-heavy in Halogen. Once you have a form spec, you can use Formless to handle all the wiring for validation, dirty states, field updates, and so on.

# Use

Install with Bower:

```sh
bower i --save purescript-halogen-formless
```

# Examples

There are several examples of forms built with Formless, from simple (a few text fields) to involved (mounting external components) to complex (implementing dependent validation and behaviors in the form).

All examples are located in the `/example` folder. You can view the code there, but there is also a fully-functioning storybook application you can use to interact with the examples.

To run the examples:

```sh
# Clone the repository to 'formless'
git clone git@github.com:thomashoneyman/purescript-halogen-formless formless

# Change into the project directory
cd formless

# Install NPM and Bower dependencies
yarn

# Build the library and examples
yarn build-all

# View the application
open dist/index.html
```

## Other Notes

This library depends on `purescript-halogen-renderless` for helper functions and the original template for the renderless component. If you would like to learn more about how to build these components, or you're ready to build your own, see the library:

- [purescript-halogen-renderless](https://github.com/thomashoneyman/purescript-halogen-renderless)

Examples are implemented using the Ocelot design system by CitizenNet. If you find this confusing, ignore the HTML and focus on the events and the data.

- [purescript-ocelot](https://github.com/citizennet/purescript-ocelot)
