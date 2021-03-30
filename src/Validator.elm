module Validator exposing
    ( Validator, custom, run, succeed, fail
    , map, map2
    , andMap, andThen
    , required, optional
    )

{-| This library helps with building custom validators of a user input.
Many validators can be chained to perform validation of a single value.
Provides interface for combining validatiors of multiple input fields with `(|>)` operator.
Allows combination of required and optional fields.


# Definition

@docs Validator, custom, run, succeed, fail


# Transform

@docs map, map2


# Applicative

@docs andMap, andThen


# Compose

@docs required, optional

-}


{-| Represents a validator of the input value. Validation can fail with a list of custom error messages.
-}
type Validator a e b
    = Validator (a -> Result (List e) b)


{-| Build a custom validator.

    intValidator : Validator String InputError Int
    intValidator =
        Validator.custom (String.toInt >> Result.fromMaybe [ NaN ])

-}
custom : (a -> Result (List e) b) -> Validator a e b
custom =
    Validator


{-| Run validator.

    intValidator : Validator String String Int
    intValidator =
        Validator.custom (String.toInt >> Result.fromMaybe [ "Not a number" ])

    Validator.run intValidator "42"
    --> (Ok 42)

    Validator.run intValidator "Fat fingers"
    --> (Err [ "Not a number" ])

-}
run : Validator a e b -> a -> Result (List e) b
run (Validator f) =
    f


{-| Create validator that always succeed.

    Validator.run (Validator.succeed 3) "blah" --> (Ok 3)

-}
succeed : b -> Validator a e b
succeed =
    Validator << always << Ok


{-| Create validator that always fails.

    Validator.run (Validator.fail "Invalid") 123 --> (Err [ "Invalid" ])

-}
fail : e -> Validator a e b
fail =
    Validator << always << Err << List.singleton



-- TRANSFORM


{-| Transform validated value with a given function.

    Validator.run (Validator.map ((+) 1) (Validator.succeed 2)) Nothing
    --> (Ok 3)

-}
map : (b -> c) -> Validator a e b -> Validator a e c
map f (Validator g) =
    Validator (Result.map f << g)


{-| Apply a function if both arguments passed validation.

    Validator.map2 (\x y -> x + y)
        (Validator.succeed 1)
        (Validator.succeed 2)
        |> (\validator -> Validator.run validator ())
        --> (Ok 3)

-}
map2 : (b -> c -> d) -> Validator a e b -> Validator a e c -> Validator a e d
map2 f (Validator g) (Validator h) =
    Validator (\x -> Result.map2 f (g x) (h x))



-- APPLICATIVE


{-| Apply wrapped function to another wrapped value.

    intValidator : Validator String String Int
    intValidator =
        Validator.custom (String.toInt >> Result.fromMaybe [ "Not a number" ])

    Validator.run (Validator.succeed ((+) 2) |> Validator.andMap intValidator) "3"
    --> (Ok 5)

-}
andMap : Validator a e b -> Validator a e (b -> c) -> Validator a e c
andMap =
    map2 (|>)


{-| Chain together many validators.

    intValidator
        |> Validator.andThen
            (\x ->
                if x < 0 then
                    Validator.fail "Must be a positive number"

                else
                    Validator.succeed x
            )
        -- ...

-}
andThen : (b -> Validator a e c) -> Validator a e b -> Validator a e c
andThen f g =
    Validator
        (\x ->
            x
                |> run g
                |> Result.map f
                |> Result.andThen (\h -> run h x)
        )



-- COMPOSE


{-| Combine validators with required field.

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

    emailValidator : Validator String String Email
    emailValidator =
        Validator.custom (parseEmail >> Result.mapError List.singleton)

    intValidator : Validator String String Int
    intValidator =
        Validator.custom (String.toInt >> Result.fromMaybe [ "Not a number" ])

    type alias FormValues r =
        { r
            | name : String
            , age : String
            , email : String
        }

    userValidator : Validator (FormValues r) String User
    userValidator =
        Validator.succeed User
            |> Validator.required .name String.isEmpty "Name is required" (Validator.custom Ok)
            |> Validator.required .age String.isEmpty "Age is required" intValidator
            |> Validator.required .email String.isEmpty "Email is required" emailValidator

    Validator.run userValidator { name = "John Doe", age = "", email = "test" }
        --> Err [ "Age is required", "Invalid email" ]

-}
required : (a -> b) -> (b -> Bool) -> e -> Validator b e c -> Validator a e (c -> d) -> Validator a e d
required accessor isEmpty err (Validator f) (Validator g) =
    Validator
        (\x ->
            x
                |> (predicateMaybe isEmpty << accessor)
                |> Result.fromMaybe [ err ]
                |> Result.andThen f
                |> applyResults (g x)
        )


{-| Combine validators with optional field.

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

    emailValidator : Validator String String Email
    emailValidator =
        Validator.custom (parseEmail >> Result.mapError List.singleton)

    intValidator : Validator String String Int
    intValidator =
        Validator.custom (String.toInt >> Result.fromMaybe [ "Not a number" ])


    type alias FormValues r =
        { r
            | name : String
            , age : String
            , email : String
        }

    userValidator : Validator (FormValues r) String User
    userValidator =
        Validator.succeed User
            |> Validator.required .name String.isEmpty "Name is required" (Validator.custom Ok)
            |> Validator.optional .age String.isEmpty 10 intValidator
            |> Validator.required .email String.isEmpty "Email is required" emailValidator

    Validator.run userValidator { name = "John Doe", age = "", email = "happy@path" }
        --> Ok (User "John Doe" 10 (Email "happy@path"))

-}
optional : (a -> b) -> (b -> Bool) -> c -> Validator b e c -> Validator a e (c -> d) -> Validator a e d
optional accessor isEmpty default (Validator f) (Validator g) =
    Validator
        (\x ->
            x
                |> (predicateMaybe isEmpty << accessor)
                |> Maybe.map f
                |> Maybe.map (applyResults (g x))
                |> Maybe.withDefault (applyResults (g x) (Ok default))
        )


applyResults : Result (List e) (a -> b) -> Result (List e) a -> Result (List e) b
applyResults r1 r2 =
    case ( r1, r2 ) of
        ( Ok f, Ok x ) ->
            Ok (f x)

        ( Err e, Err e_ ) ->
            Err (e ++ e_)

        ( Err e, _ ) ->
            Err e

        ( _, Err e ) ->
            Err e


predicateMaybe : (a -> Bool) -> a -> Maybe a
predicateMaybe isEmpty x =
    if isEmpty x then
        Nothing

    else
        Just x
