# validation-aeson

## Goals

* Lifting validation into the type system using the `validation` library
* Adding logging information to make error messages easier to read
* Tracking all aeson's errors using `AccValidation`

## Notes

* `BihistoricalError` tracks:
  * Log used for the parser
  * Log used for the user's domain
  * Parsing error (Left)
  * Validation error (Right)

## TODO

* Support for lists and vectors using `sequenceA`
* Support for `Maybe a`
* Naming `BihistoricalError` to something better? D:
* Preventing "Key not found" and "Incorrect type" errors
  * Additional parsing functions to catch aeson's incorrect type errors
  * Adding instances of `FromJSON` for primitive types that can catch incorrect type errors