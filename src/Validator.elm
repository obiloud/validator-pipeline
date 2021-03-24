module Validator exposing
    ( Validator, run
    , map, map2
    , andMap, andThen
    , succeed, fail, custom
    , required, optional
    )

{-|


# Definition

@docs Validator, run


# Transform

@docs map, map2


# Applicative

@docs andMap, andThen

@docs succeed, fail, custom


# Compose

@docs required, optional

-}


{-| Validator
-}
type Validator a e b
    = Validator (a -> Result (List e) b)


{-| Run validator
-}
run : Validator a e b -> a -> Result (List e) b
run (Validator f) =
    f


{-| Create validator that always succeed
-}
succeed : b -> Validator a e b
succeed =
    Validator << always << Ok


{-| Create validator that always fails
-}
fail : e -> Validator a e b
fail =
    Validator << always << Err << List.singleton


{-| Build custom validator
-}
custom : (a -> Result (List e) b) -> Validator a e b
custom =
    Validator


{-| Transform validated value
-}
map : (b -> c) -> Validator a e b -> Validator a e c
map f (Validator g) =
    Validator (Result.map f << g)


{-| Transform validated value
-}
map2 : (b -> c -> d) -> Validator a e b -> Validator a e c -> Validator a e d
map2 f (Validator g) (Validator h) =
    Validator (\x -> Result.map2 f (g x) (h x))


{-| Apply wrapped function on to another wrapped value
-}
andMap : Validator a e b -> Validator a e (b -> c) -> Validator a e c
andMap =
    map2 (|>)


{-| Chain validators
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


{-| Combine validators with required field
-}
required : (a -> b) -> (b -> Bool) -> e -> Validator b e c -> Validator a e (c -> d) -> Validator a e d
required accessor isEmpty err (Validator f) (Validator h) =
    Validator
        (\x ->
            x
                |> (predicateMaybe isEmpty << accessor)
                |> Result.fromMaybe [ err ]
                |> Result.andThen f
                |> applyResults (h x)
        )


{-| Combine validators with optional field
-}
optional : (a -> b) -> (b -> Bool) -> c -> Validator b e c -> Validator a e (c -> d) -> Validator a e d
optional accessor isEmpty default (Validator f) (Validator h) =
    Validator
        (\x ->
            x
                |> (predicateMaybe isEmpty << accessor)
                |> Maybe.map f
                |> Maybe.map (applyResults (h x))
                |> Maybe.withDefault (applyResults (h x) (Ok default))
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
