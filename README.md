## json-validation ([documentation](https://mitchellwrosen.github.io/json-validation/))
[![Build Status](https://travis-ci.org/mitchellwrosen/json-validation.svg?branch=master)](https://travis-ci.org/mitchellwrosen/json-validation)

A small, simple JSON validation library. There are already a few JSON schema frameworks in the ecosystem:

- [`json-schema`](https://hackage.haskell.org/package/json-schema)
- [`aeson-schema`](https://hackage.haskell.org/package/aeson-schema)
- [`hjsonschema`](https://hackage.haskell.org/package/hjsonschema)

Of these, this library is most similar to `json-schema`, in that it is not an implementation of [json-schema.org](http://json-schema.org/), but rather is a collection of combinators for validating the structure of a JSON object (e.g. for use in [`hspec-expectations`](https://hackage.haskell.org/package/hspec-expectations)).

## TODOs

- More tests
- Steal some pre-defined formats from http://json-schema.org/latest/json-schema-validation.html#anchor104
