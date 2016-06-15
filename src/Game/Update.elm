module Game.Update
    exposing
        ( Result
        , Update
        , returnAlive
        , returnMaybe
        , returnDead
        , fromEffects
        , runOnMaybe
        , runIf
        , andThen
        , filterAlive
        )

{-| When updating a component in a game, frequently you will want to have some state
for representing the death/destruction of the component. `Game.Update` offers you
convenient operations for chaining together update functions that have side-effects
and could kill the object, representing the alive state as `Just object` and
the death state as `Nothing`.

# Types

@docs Result, Update

# Constructors

@docs returnAlive, returnMaybe, returnDead, fromEffects

# Running Updates

@docs runOnMaybe, runIf

# Chaining Updates

@docs andThen

# Filtering results

@docs filterAlive
-}

-- LOCAL IMPORTS

import Effects exposing (Effects)


-- TYPES


{-| A `Result a effect` represents the result from updating component of
type `a` with side-effects of type `effect`. The component might be dead
or alive after this update has occurred.

A `Result a effect` is equivalent to an `Effects (Maybe a) effect` from
[elm-effects](http://package.elm-lang.org/packages/akbiggs/elm-effects), which means
that you can chain and handle effects from it using the operations from that package.
-}
type alias Result a effect =
    Effects (Maybe a) effect


{-| An `Update a effect` represents a function that takes a component
of type `a` and produces an updated version of that component with side-effects
of type `effect`. The component might be dead or alive after this update has
occurred.
-}
type alias Update a effect =
    a -> Result a effect



-- CONSTRUCTORS


{-| Takes a value and returns a `Result` with no side-effects
indicating that the object is still alive.
-}
returnAlive : a -> Result a effect
returnAlive x =
    Effects.return (Just x)


{-| Takes a `Maybe` and returns a `Result` with no side-effects indicating that the object is
alive if the `Maybe` has a value, otherwise dead.
-}
returnMaybe : Maybe a -> Result a effect
returnMaybe maybeX =
    Effects.return maybeX


{-| Returns a result with no side-effects indicating a dead object.
-}
returnDead : Result a effect
returnDead =
    Effects.return Nothing


{-| Takes an `Effects` object and converts it to an update result,
reporting that the value is alive.
-}
fromEffects : Effects a effect -> Result a effect
fromEffects ( x, effects ) =
    ( Just x, effects )



-- RUNNING UPDATES


{-| Updates an object if the object is alive(the `Maybe` has a value).
Otherwise, returns a `Result` with no side-effects indicating the object is dead.
-}
runOnMaybe : Update a effect -> Maybe a -> Result a effect
runOnMaybe updateFn maybeObj =
    Maybe.map updateFn maybeObj
        |> Maybe.withDefault ( Nothing, [] )


{-| Runs an update on an alive object if the condition is true. Otherwise, returns
a `Result` indicating that the object is alive.
-}
runIf : Bool -> Update a effect -> a -> Result a effect
runIf cond updateFn obj =
    if cond then
        updateFn obj
    else
        returnAlive obj


{-| Takes the result from one update and runs another update function on it
if the object in the result is still alive. However, if the object is dead,
the update will not occur and no additional side-effects will be added to
the result. This is useful when you want to run a bunch of functions on an object,
any of which could kill the object.
-}
andThen : Result a effect -> Update a effect -> Result a effect
andThen result updateFn =
    result `Effects.andThen` runOnMaybe updateFn


{-| Take a bunch of update results and filter out the ones that are still alive.
To preserve the effects from the dead objects, the effects are all batched together
into a single list instead of being kept as separate lists.
-}
filterAlive : List (Result a effect) -> Effects (List a) effect
filterAlive results =
    let
        ( maybeValues, effects ) =
            Effects.batch results
    in
        Effects.init (List.filterMap identity maybeValues) effects
