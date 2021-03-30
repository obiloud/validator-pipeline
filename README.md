# validator-pipeline
Build custom validators of a user input. Combine validatiors of multiple input fields with `(|>)` operator.

## Why yet another validator package?
Form validation is very common and there is already number of different libraries to choose from: 
* [arowM/elm-from-validator](https://package.elm-lang.org/packages/arowM/elm-form-validator/latest/)
* [etaque/elm-form](https://package.elm-lang.org/packages/etaque/elm-form/latest/)
* [gage251/elm-validator-pipeline](https://package.elm-lang.org/packages/gege251/elm-validator-pipeline/latest/)
* [jaredramirez/elm-field](https://package.elm-lang.org/packages/jaredramirez/elm-field/latest/)
* [Bastes/the-validator](https://package.elm-lang.org/packages/Bastes/the-validator/latest/)

What sets this library appart from others is intuitive interface for working with optional fields.
It is not enough to simply leave certain fields unchecked. This library exposes interface to provide default value if optional field is empty but validate the same field otherwise.

Library is very flexibile and leaves implementation of the custom validator to the developer. Validation is decoupled from any kind of representation and can be used both inside the view function and in the update function.

## Examples

Example validating a record with form values.

```elm
import Dict exposing (Dict)
import Validator exposing (Validator)


type alias User =
    { name : String
    , age : Int
    , email : Email
    }


type Email = Email String


parseEmail : String -> Result String Email
parseEmail str =
    if String.contains "@" str then
        Ok (Email str)
    else
        Err "Invalid email"


emailValidator : String -> Validator String (String, String) Email
emailValidator fieldName =
    Validator.custom (parseEmail >> Result.mapError (Tuple.pair fieldName >> List.singleton))


intValidator : String -> Validator String (String, String) Int
intValidator fieldName =
    Validator.custom (String.toInt >> Result.fromMaybe [ (fieldName, "Not a number") ])


type alias FormValues =
    { name : String
    , age : String
    , email : String
    }


userValidator : Validator FormValues (String, String) User
userValidator =
    Validator.succeed User
        |> Validator.required .name String.isEmpty ("name", "Name is required") (Validator.custom Ok)
        |> Validator.optional .age String.isEmpty 10 (intValidator "age")
        |> Validator.required .email String.isEmpty ("email", "Email is required") (emailValidator "email")


model : FormValues
model =
    { name = "John Doe"
    , age = ""
    , email = "wrong address"
    }


validationErrors : Dict String String
validationErrors =
    case Validator.run userValidator model of 
        Ok _ ->
            Dict.empty
        Err errs ->
            Dict.fromList errs
       

Dict.get "name" validationErrors --> Nothing


Dict.get "age" validationErrors --> Nothing


Dict.get "email" validationErrors --> (Just "Invalid email")
        
```