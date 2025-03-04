# Reacl-c-basics

This is a collection of utilities often useful when developing with
the [Reacl-C](https://github.com/active-group/reacl-c) web programming
library for ClojureScript

[![Latest Version](https://img.shields.io/clojars/v/de.active-group/reacl-c-basics.svg)](https://clojars.org/de.active-group/reacl-c-basics)
[![Tests Status](https://github.com/active-group/reacl-c-basics/workflows/ci/badge.svg)](https://github.com/active-group/reacl-c-basics/actions)

## Documentation

The API documentation for the latest release are available [here](https://cljdoc.xyz/d/de.active-group/reacl-c-basics/CURRENT).

## The contents at a glance

The following sections give a brief overview of the most used
namespaces of the library.

### Forms and input elements

The namespace
[`reacl-c-basics.forms.core`](https://cljdoc.xyz/d/de.active-group/reacl-c-basics/CURRENT/api/reacl-c-basics.forms.core)
contains functions very similar to the corresponding DOM elements
`input`, `textarea`, `select` and `form`, but with the most relevant
state (the value entered by the user) as their item state.

The input elements also support the `:multiple` attribute when
possible, and special new attributes are added for the [HTML5 Form
Validation
API](https://developer.mozilla.org/en-US/docs/Learn/Forms/Form_validation).

Note: `reacl-c-basics.forms` is deprecated.

#### Advanced and custom input types

The namespace
[`reacl-c-basics.forms.types`](https://cljdoc.xyz/d/de.active-group/reacl-c-basics/CURRENT/api/reacl-c-basics.forms.types)
contains additional values - called *types* - that can be used for the
`:type` attribute of the `input` items of
`reacl-c-basics.forms.core`. It also contains functions to create new
types.

The most used types are: `opt-integer`, which changes the state of the
input item to either `nil` or an integer number, and `opt-fixnum`,
which changes enables the user to enter a floating point number, but
only with a fixed number of decimals. 

Some of the type values, will actually change the
`reacl-c-basics.forms.core/input` item into a textbox or select item,
namely `multiline-string` and `enum` or `string-enum` respectively.

### Ajax requests

The namespace
[`reacl-c-basics.ajax`](https://cljdoc.xyz/d/de.active-group/reacl-c-basics/CURRENT/api/reacl-c-basics.ajax)
gives you mostly invisible items that perform an Ajax request and give
you easy access to the results.

### Client side routing

For an easy setup of HTML5 Client Side Routing, look at the namespaces
starting with
[`reacl-c-basics.pages`](https://cljdoc.xyz/d/de.active-group/reacl-c-basics/CURRENT/api/reacl-c-basics.pages).

### Timers and animation frames

The core namespace of the library
[`reacl-c-basics.core`](https://cljdoc.xyz/d/de.active-group/reacl-c-basics/CURRENT/api/reacl-c-basics.core)
contains useful subscriptions to a JavaScript timer (`timeout` and
`interval`), and to animation frame timestamps (`animation-frame` and
`animation-frames`).
