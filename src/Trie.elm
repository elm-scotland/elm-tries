module Trie exposing
    ( Trie
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


{-| A trie mapping keys to values, where the keys are `List comparable`.

A `Trie` is a lot like a `Dict` except the keys are expanded into lists. Keys
that have common lists as suffixes share space, and it is possible to efficiently
search for keys matching a particular suffix.

-}
type Trie comparable a
    = Trie (Maybe a) (Dict comparable (Trie comparable a))


{-| Create an empty trie.
-}
empty : Trie comparable a
empty =
    Trie Nothing Dict.empty


{-| Create a trie with one key-value pair.
-}
singleton : List comparable -> a -> Trie comparable a
singleton key val =
    case key of
        [] ->
            Trie (Just val) Dict.empty

        head :: tail ->
            Trie Nothing (Dict.singleton head (singleton tail val))


{-| Insert a key-value pair into a trie. Replaces value when there is
a collision.
-}
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


{-| Update the value of a trie for a specific key with a given function.
-}
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


{-| Remove a key-value pair from a trie. If the key is not found,
no changes are made.
-}
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


{-| Determine if a trie is empty.

    `isEmpty empty == True`

-}
isEmpty : Trie comparable a -> Bool
isEmpty trie =
    size trie == 0


{-| TODO: I think this will match key suffixes? Probably should not for the Dict API.
-}
member : List comparable -> Trie comparable a -> Bool
member key trie =
    case subtrie key trie of
        Nothing ->
            False

        Just (Trie maybeValue _) ->
            isJust maybeValue


{-| Get the value associated with a key. If the key is not found, return
`Nothing`.
-}
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


{-| Determine the number of key-value pairs in the trie.
-}
size : Trie comparable a -> Int
size trie =
    foldl (\_ _ accum -> accum + 1) 0 trie


{-| Get all of the keys in a trie, sorted from lowest to highest.
-}
keys : Trie comparable a -> List (List comparable)
keys trie =
    foldl (\key value keyList -> key :: keyList) [] trie


{-| Get all of the values in a trie, in the order of their keys.
-}
values : Trie comparable a -> List a
values trie =
    foldl (\key value valueList -> value :: valueList) [] trie


{-| Convert a trie into an association list of key-value pairs, sorted by keys.
-}
toList : Trie comparable a -> List ( List comparable, a )
toList trie =
    foldl (\key value list -> ( key, value ) :: list) [] trie


{-| Convert an association list into a trie.
-}
fromList : List ( List comparable, a ) -> Trie comparable a
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs


{-| Apply a function to all values in a trie.
-}
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


{-| Fold over the key-value pairs in a trie from lowest key to highest key.
-}
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


{-| Fold over the key-value pairs in a trie from highest key to lowest key.

Due to the way shorter keys are nearer the top of the trie this fold function
has to hold more pending nodes in memory in order to fold in order from the
highest key to the lowest key. For this reason it is less efficient than `foldl`
and `foldl` should be preferred unless the ordering is important.

-}
foldr : (List comparable -> a -> b -> b) -> b -> Trie comparable a -> b
foldr fn accum trie =
    foldrInner
        (\keyPath maybeValue innerAccum ->
            case maybeValue of
                Nothing ->
                    innerAccum

                Just value ->
                    fn keyPath value innerAccum
        )
        accum
        [ ( [], trie, False ) ]


foldrInner :
    (List comparable -> Maybe a -> b -> b)
    -> b
    -> List ( List comparable, Trie comparable a, Bool )
    -> b
foldrInner fn accum trail =
    case trail of
        [] ->
            accum

        ( keyPath, Trie maybeValue dict, expanded ) :: remaining ->
            case expanded of
                True ->
                    let
                        nextAccum =
                            fn (List.reverse keyPath) maybeValue accum
                    in
                    foldrInner fn nextAccum remaining

                False ->
                    let
                        nextTrail =
                            Dict.foldl (\k trie steps -> ( k :: keyPath, trie, False ) :: steps)
                                (( keyPath, Trie maybeValue dict, True ) :: remaining)
                                dict
                    in
                    foldrInner fn accum nextTrail


{-| Keep only the key-value pairs that pass the given test.
-}
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


{-| Partition a trie according to some test. The first trie contains all
key-value pairs which passed the test, and the second contains the pairs that
did not.
-}
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


{-| Combine two tries. If there is a collision, preference is given to the first
trie.
-}
union : Trie comparable a -> Trie comparable a -> Trie comparable a
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second trie. Preference is
given to values in the first dictionary.
-}
intersect : Trie comparable a -> Trie comparable a -> Trie comparable a
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second trie.
-}
diff : Trie comparable a -> Trie comparable b -> Trie comparable a
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| The most general way of combining two tries. You provide three accumulators
for when a given key appears:

1.  Only in the left trie.
2.  In both tries.
3.  Only in the right trie.

You then traverse all the keys from lowest to highest, building up whatever you
want.

-}
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


{-| Given a suffix, finds all keys that begin with that suffix.
-}
expand : List comparable -> Trie comparable a -> List (List comparable)
expand key trie =
    case subtrie key trie of
        Nothing ->
            []

        Just innerTrie ->
            foldl (\innerKey _ accum -> (key ++ innerKey) :: accum) [] innerTrie


{-| Given a suffix, checks if there are keys that begin with that suffix.
-}
matches : List comparable -> Trie comparable a -> Bool
matches key trie =
    case subtrie key trie of
        Nothing ->
            False

        Just (Trie maybeValue _) ->
            isJust maybeValue


{-| Given a suffix, finds any sub-trie containing the key-value pairs where the
original keys begin with that suffix. The keys in the sub-trie will only consist
of the remaining portion of the key after the suffix.
-}
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


{-| `Match` describes how a flexible search over a trie will proceed.

`Break` - do not explore any more below the current suffix.
`Wildcard` - continue with all possible next keys below the current suffix.
`ContinueIf` - continue with the next key provided it exactly matches the comparable specified.
`ContinueIfOneOf` - continue with the next key provided it matches one of the comparables specified.

The `Break`, `ContinueIf` and `ContinueIfOneOf` options allow a trie to be
traveresed efficiently without exploring unnecessary keys.

The `Wildcard` and `ContinueIfOneOf` options allow flexible matching within a
trie. Functions such as case-insensitive matching, fuzzy matching or regular
expression matching can be implemented using these options.

-}
type Match comparable
    = Break
    | Wildcard
    | ContinueIf comparable
    | ContinueIfOneOf (List comparable)


{-| Performs a flexible matching fold over a trie from the lowest to the highest
key in order.

Suppose the function passed in has this form:

`searchFn maybeKeyPart maybeValue context accum = ...`

The `maybeKeyPart` parameter will be set to the next item from the key being
scanned as a list. This is a `Maybe` as the empty list can be a key in a trie.
In practice the value `Nothing` will only be passed to this function on the
first call when the empty key is present.

The `maybeValue` parameter will be set to any value found at the current position
in the trie.

The `context` parameter will be held against the particular node in the trie
being explored. When and if that node is returned to in order to explore other
key paths in the trie, the context for that node will be restored. The trie is
explored using a depth first search, and the contexts are held in a stack of
pending nodes to explore. An example use of the context might be to hold the
remaining portion of a key to be matched.

The `accum` parameter is used like the accumulator in a fold, it can be updated
on each matching key found.

-}
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



-- Helper functions.


isJust : Maybe a -> Bool
isJust maybeSomething =
    case maybeSomething of
        Nothing ->
            False

        Just _ ->
            True
