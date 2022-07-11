# Reacl-c-basics

This is a collection of utilities often useful when developing with
the [Reacl-C](https://github.com/active-group/reacl-c) web programming
library for ClojureScript

[![Latest Version](https://img.shields.io/clojars/v/de.active-group/reacl-c.svg)](https://clojars.org/de.active-group/reacl-c-basics)
[![Tests Status](https://github.com/active-group/reacl-c-basics/workflows/Tests/badge.svg)](https://github.com/active-group/reacl-c-basics/actions)

## Documentation

The API documentation for the latest release are available [here](https://cljdoc.xyz/d/de.active-group/reacl-c-basics/CURRENT).

## The contents at a glance

### Forms and input elements

The namespace `reacl-c-basics.forms.core` contains functions very
similar to the corresponding DOM elements `input`, `textarea`,
`select` and `form`, but with the most relevant state (the value
entered by the user) as their state.

For `input`, the state depends on the `:type` attribute:

| :type             | State of the item         |
| ----------------- | ------------------------- |
| `checkbox`        | A boolean                 |
| `radio`           | A boolean                 |
| `file`            | A `js/File` object or nil |
| `submit`/`cancel` | *State is ignored*        |
| *otherwise*       | A string                  |

For types that support it, the `:multiple` attribute will change the
state to a list of the listed types of values.

The `select` together with the `option` replacement supports arbitrary
Clojure values as the value behind the text that is shown to the user
in the dropdown element. The `:multiple` attribute is also properly
supported.

All user input elements support new custom validation attributes (see
[HTML5 Form Validation
API](https://developer.mozilla.org/en-US/docs/Learn/Forms/Form_validation)):

- `:invalid` can be set to a string to mark the input as invalid and
  set a custom validation message, and to nil to mark it as valid
  again.
- `:validate` can be set to a function that is called with the state
  of the item, and can return nil or a string for the same purpose.
- `:report-validity` can be set on input elements and the `form` item,
  to show validation errors to the users (which usually happens before
  the form is submitted automatically).

Note: `reacl-c-basics.forms` is deprecated.

#### Advanced and custom input types

The namespace `reacl-c-basics.forms.types` contains additional
values - called *types* - that can be used for the `:type` attribute
of the `input` items of `reacl-c-basics.forms.core`. It also contains
functions to create new types.

The most used types are: `opt-integer`, which changes the state of the
input item to either `nil` or an integer number, and `opt-fixnum`,
which changes enables the user to enter a floating point number, but
only with a fixed number of decimals. 

The function `native-type` takes a string and gives you a type that
results in the same kind of input that you get when you use that
string as the type directly, e.g. `string` is the same as `(native-type
"text")`.

Some of the type values, will actually change the
`reacl-c-basics.forms.core/input` item into a textbox or select item,
namely `multiline-string` and `enum` or `string-enum` respectively.

### Ajax requests

The namespace `reacl-c-basics.ajax` gives you mostly invisible items
that perform an Ajax request and give you easy access to the results.

You define requests usually with `GET`, `POST`, and maybe
`map-ok-response`. Then you can execute them with the `fetch` item
which changes its state to a response value when the server has
responded. Or you use the `delivery` item and the `deliver` action to
execute requests as a reaction to certain events.

All items cancel outstanding requests when they are removed from the
dom tree.

### Client side routing

For an easy setup of HTML5 Client Side Routing, look at the namespaces
starting with `reacl-c-basics.pages`.

As a short intro, write

- a `cljc` file that defines the routes of your app with
  `reacl-c-basics.pages.routes`

- a router that shows different content depending on the current
  location using `reacl-c-basics.pages.router`

- a ring handler that serves the client for all your routes with
  `reacl-c-basics.pages.ring`

### Timers and animation frames

The core namespace of the library `reacl-c-basics.core` contains
useful subscriptions to a JavaScript timer (`timeout` and `interval`),
and to animation frame timestamps (`animation-frame` and
`animation-frames`).

