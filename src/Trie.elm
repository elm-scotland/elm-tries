module Trie exposing
    ( Trie(..)
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , Match(..), match, expand, matches, subtrie
    )

{-| A trie mapping unique strings to values.


# Data structure

@docs Trie


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge


# Trie specific search operations.

@docs Match, match, expand, matches, subtrie

-}

import Dict exposing (Dict)


type Trie comparable a
    = Trie (Maybe a) (Dict comparable (Trie comparable a))


empty : Trie comparable a
empty =
    Trie Nothing Dict.empty


singleton : List comparable -> a -> Trie comparable a
singleton key val =
    case key of
        [] ->
            Trie (Just val) Dict.empty

        head :: tail ->
            Trie Nothing (Dict.singleton head (singleton tail val))


insert : List comparable -> a -> Trie comparable a -> Trie comparable a
insert key val trie =
    case ( key, trie ) of
        ( [], Trie _ rem ) ->
            Trie (Just val) rem

        ( head :: tail, Trie maybeSomeVal rem ) ->
            Trie maybeSomeVal
                (Dict.update head
                    (\maybeTrie ->
                        case maybeTrie of
                            Nothing ->
                                singleton tail val |> Just

                            Just existingTrie ->
                                insert tail val existingTrie |> Just
                    )
                    rem
                )


update : List comparable -> (Maybe a -> Maybe a) -> Trie comparable a -> Trie comparable a
update key fn trie =
    case ( key, trie ) of
        ( [], Trie maybeVal rem ) ->
            Trie (fn maybeVal) rem

        ( head :: tail, Trie maybeSomeVal rem ) ->
            Trie maybeSomeVal
                (Dict.update head
                    (\maybeTrie ->
                        case maybeTrie of
                            Nothing ->
                                case fn Nothing of
                                    Nothing ->
                                        Just empty

                                    Just fnVal ->
                                        singleton tail fnVal |> Just

                            Just existingTrie ->
                                update tail fn existingTrie |> Just
                    )
                    rem
                )


remove : List comparable -> Trie comparable a -> Trie comparable a
remove key trie =
    case ( key, trie ) of
        ( [], Trie _ rem ) ->
            Trie Nothing rem

        ( head :: tail, Trie maybeSomeVal rem ) ->
            Trie maybeSomeVal
                (Dict.update head
                    (\maybeTrie ->
                        case maybeTrie of
                            Nothing ->
                                Nothing

                            Just existingTrie ->
                                remove tail existingTrie |> Just
                    )
                    rem
                )


isEmpty : Trie comparable a -> Bool
isEmpty trie =
    size trie == 0


member : List comparable -> Trie comparable a -> Bool
member key trie =
    case subtrie key trie of
        Nothing ->
            False

        Just (Trie maybeValue _) ->
            isJust maybeValue


get : List comparable -> Trie comparable a -> Maybe a
get key trie =
    case ( key, trie ) of
        ( [], Trie maybeSomeVal _ ) ->
            maybeSomeVal

        ( head :: tail, Trie _ rem ) ->
            let
                maybeTrie =
                    Dict.get head rem
            in
            case maybeTrie of
                Nothing ->
                    Nothing

                Just subTrie ->
                    get tail subTrie


size : Trie comparable a -> Int
size trie =
    foldr (\_ _ accum -> accum + 1) 0 trie


keys : Trie comparable a -> List (List comparable)
keys trie =
    foldr (\key value keyList -> key :: keyList) [] trie


values : Trie comparable a -> List a
values trie =
    foldr (\key value valueList -> value :: valueList) [] trie


toList : Trie comparable a -> List ( List comparable, a )
toList trie =
    foldr (\key value list -> ( key, value ) :: list) [] trie


fromList : List ( List comparable, a ) -> Trie comparable a
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs


map : (List comparable -> a -> b) -> Trie comparable a -> Trie comparable b
map fn trie =
    mapInner (\chars -> fn <| List.reverse chars) [] trie


mapInner : (List comparable -> a -> b) -> List comparable -> Trie comparable a -> Trie comparable b
mapInner fn keyAccum ((Trie maybeValue dict) as trie) =
    case maybeValue of
        Nothing ->
            Trie Nothing (Dict.map (\key innerTrie -> mapInner fn (key :: keyAccum) innerTrie) dict)

        Just value ->
            Trie (Just <| fn keyAccum value) (Dict.map (\key innerTrie -> mapInner fn (key :: keyAccum) innerTrie) dict)


foldl : (List comparable -> a -> b -> b) -> b -> Trie comparable a -> b
foldl fn accum trie =
    match
        (\maybeKeyPart maybeValue ctx innerAccum ->
            let
                nextCtx =
                    case maybeKeyPart of
                        Nothing ->
                            ctx

                        Just k ->
                            k :: ctx
            in
            case maybeValue of
                Nothing ->
                    ( innerAccum, nextCtx, Wildcard )

                Just value ->
                    ( fn (List.reverse nextCtx) value innerAccum, nextCtx, Wildcard )
        )
        accum
        []
        trie


foldr : (List comparable -> a -> b -> b) -> b -> Trie comparable a -> b
foldr fn accum trie =
    matchr
        (\maybeKeyPart maybeValue ctx innerAccum ->
            let
                nextCtx =
                    case maybeKeyPart of
                        Nothing ->
                            ctx

                        Just k ->
                            k :: ctx
            in
            case maybeValue of
                Nothing ->
                    ( innerAccum, nextCtx, Wildcard )

                Just value ->
                    ( fn (List.reverse nextCtx) value innerAccum, nextCtx, Wildcard )
        )
        accum
        []
        trie


filter : (List comparable -> a -> Bool) -> Trie comparable a -> Trie comparable a
filter isGood trie =
    foldl
        (\k v d ->
            if isGood k v then
                insert k v d

            else
                d
        )
        empty
        trie


partition : (List comparable -> a -> Bool) -> Trie comparable a -> ( Trie comparable a, Trie comparable a )
partition isGood trie =
    let
        add key value ( t1, t2 ) =
            if isGood key value then
                ( insert key value t1, t2 )

            else
                ( t1, insert key value t2 )
    in
    foldl add ( empty, empty ) trie


union : Trie comparable a -> Trie comparable a -> Trie comparable a
union t1 t2 =
    foldl insert t2 t1


intersect : Trie comparable a -> Trie comparable a -> Trie comparable a
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


diff : Trie comparable a -> Trie comparable b -> Trie comparable a
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


merge :
    (List comparable -> a -> result -> result)
    -> (List comparable -> a -> b -> result -> result)
    -> (List comparable -> b -> result -> result)
    -> Trie comparable a
    -> Trie comparable b
    -> result
    -> result
merge leftStep bothStep rightStep leftTrie rightTrie initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if lKey < rKey then
                        stepState rKey rValue ( rest, leftStep lKey lValue result )

                    else if lKey > rKey then
                        ( list, rightStep rKey rValue result )

                    else
                        ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList leftTrie, initialResult ) rightTrie
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers


expand : List comparable -> Trie comparable a -> List (List comparable)
expand key trie =
    case subtrie key trie of
        Nothing ->
            []

        Just innerTrie ->
            foldr (\innerKey _ accum -> (key ++ innerKey) :: accum) [] innerTrie


matches : List comparable -> Trie comparable a -> Bool
matches key trie =
    case subtrie key trie of
        Nothing ->
            False

        Just (Trie maybeValue _) ->
            isJust maybeValue


subtrie : List comparable -> Trie comparable a -> Maybe (Trie comparable a)
subtrie key ((Trie maybeValue dict) as trie) =
    case key of
        [] ->
            Just trie

        k :: ks ->
            case Dict.get k dict of
                Nothing ->
                    Nothing

                Just innerTrie ->
                    subtrie ks innerTrie



-- Flexible search API


type Match comparable
    = Break
    | Wildcard
    | ContinueIf comparable
    | ContinueIfOneOf (List comparable)


match :
    (Maybe comparable -> Maybe a -> context -> b -> ( b, context, Match comparable ))
    -> b
    -> context
    -> Trie comparable a
    -> b
match fn accum context trie =
    matchInner fn accum [ ( [], context, trie ) ]


matchInner :
    (Maybe comparable -> Maybe a -> context -> b -> ( b, context, Match comparable ))
    -> b
    -> List ( List comparable, context, Trie comparable a )
    -> b
matchInner fn accum trail =
    -- Check the head of the trail.
    -- If there is a head, process it through fn.
    -- No head, done
    -- Check the result of fn.
    -- Expand onto the trail unless its Break.
    -- Recurse on matchInner.
    case trail of
        [] ->
            accum

        ( keyPath, context, Trie maybeValue dict ) :: remaining ->
            let
                ( nextAccum, nextContext, nextMatch ) =
                    fn (List.head keyPath) maybeValue context accum
            in
            case nextMatch of
                Break ->
                    matchInner fn nextAccum remaining

                Wildcard ->
                    let
                        nextTrail =
                            Dict.foldr (\k trie steps -> ( k :: keyPath, nextContext, trie ) :: steps) remaining dict
                    in
                    matchInner fn nextAccum nextTrail

                ContinueIf k ->
                    let
                        nextTrail =
                            case Dict.get k dict of
                                Nothing ->
                                    remaining

                                Just trie ->
                                    ( k :: keyPath, nextContext, trie ) :: remaining
                    in
                    matchInner fn nextAccum nextTrail

                ContinueIfOneOf list ->
                    let
                        nextTrail =
                            List.foldl
                                (\k steps ->
                                    case Dict.get k dict of
                                        Nothing ->
                                            steps

                                        Just trie ->
                                            ( k :: keyPath, nextContext, trie ) :: steps
                                )
                                remaining
                                list
                    in
                    matchInner fn nextAccum nextTrail


matchr :
    (Maybe comparable -> Maybe a -> context -> b -> ( b, context, Match comparable ))
    -> b
    -> context
    -> Trie comparable a
    -> b
matchr fn accum context trie =
    matchrInner fn accum [ ( [], context, trie ) ]


matchrInner :
    (Maybe comparable -> Maybe a -> context -> b -> ( b, context, Match comparable ))
    -> b
    -> List ( List comparable, context, Trie comparable a )
    -> b
matchrInner fn accum trail =
    -- Check the head of the trail.
    -- If there is a head, process it through fn.
    -- No head, done
    -- Check the result of fn.
    -- Expand onto the trail unless its Break.
    -- Recurse on matchInner.
    case trail of
        [] ->
            accum

        ( keyPath, context, Trie maybeValue dict ) :: remaining ->
            let
                ( nextAccum, nextContext, nextMatch ) =
                    fn (List.head keyPath) maybeValue context accum
            in
            case nextMatch of
                Break ->
                    matchInner fn nextAccum remaining

                Wildcard ->
                    let
                        nextTrail =
                            Dict.foldr (\k trie steps -> ( k :: keyPath, nextContext, trie ) :: steps) remaining dict
                    in
                    matchInner fn nextAccum nextTrail

                ContinueIf k ->
                    let
                        nextTrail =
                            case Dict.get k dict of
                                Nothing ->
                                    remaining

                                Just trie ->
                                    ( k :: keyPath, nextContext, trie ) :: remaining
                    in
                    matchInner fn nextAccum nextTrail

                ContinueIfOneOf list ->
                    let
                        nextTrail =
                            List.foldl
                                (\k steps ->
                                    case Dict.get k dict of
                                        Nothing ->
                                            steps

                                        Just trie ->
                                            ( k :: keyPath, nextContext, trie ) :: steps
                                )
                                remaining
                                list
                    in
                    matchInner fn nextAccum nextTrail



-- Helper functions.


isJust : Maybe a -> Bool
isJust maybeSomething =
    case maybeSomething of
        Nothing ->
            False

        Just _ ->
            True
