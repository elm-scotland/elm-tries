module Trie exposing
    ( Trie(..)
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , expand, matches, subtrie
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


# Trie specific string matching operations

@docs expand, matches, subtrie

-}

import Dict exposing (Dict)
import Search



-- Ideas
-- expandIgnoreCase : String -> Trie comparable a -> List String
-- matchesIgnoreCase : String -> Trie comparable a -> Bool
-- match : (List Char -> Char -> a -> Bool) -> Trie comparable a -> Bool
-- match _ _ =
--     Debug.todo "map"


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


map : (List comparable -> a -> b) -> List comparable -> Trie comparable a -> Trie comparable b
map fn keyAccum ((Trie maybeValue dict) as trie) =
    case maybeValue of
        Nothing ->
            Trie Nothing (Dict.map (\key innerTrie -> map fn (key :: keyAccum) innerTrie) dict)

        Just value ->
            Trie (Just <| fn keyAccum value) (Dict.map (\key innerTrie -> map fn (key :: keyAccum) innerTrie) dict)


foldl : (List comparable -> a -> b -> b) -> b -> Trie comparable a -> b
foldl fn accum ((Trie maybeValue _) as trie) =
    Search.depthFirst { step = wildcardStepl, cost = \_ -> 1.0 }
        [ ( ( [], trie ), isJust maybeValue ) ]
        |> foldSearchGoals fn accum


foldr : (List comparable -> a -> b -> b) -> b -> Trie comparable a -> b
foldr fn accum ((Trie maybeValue _) as trie) =
    Search.depthFirst { step = wildcardStepr, cost = \_ -> 1.0 }
        [ ( ( [], trie ), isJust maybeValue ) ]
        |> foldSearchGoals fn accum


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



-- Helper functions.


isJust : Maybe a -> Bool
isJust maybeSomething =
    case maybeSomething of
        Nothing ->
            False

        Just _ ->
            True


{-| Expands all possible extensions of the current key path in a trie.

The `Dict` containing the child tries is folded left in this implementation.

-}
wildcardStepl : ( List comparable, Trie comparable a ) -> List ( ( List comparable, Trie comparable a ), Bool )
wildcardStepl ( keyAccum, Trie _ dict ) =
    Dict.foldl
        (\k ((Trie maybeValue _) as innerTrie) stack ->
            ( ( k :: keyAccum, innerTrie ), isJust maybeValue )
                :: stack
        )
        []
        dict


{-| Expands all possible extensions of the current key path in a trie.

The `Dict` containing the child tries is folded right in this implementation.

-}
wildcardStepr : ( List comparable, Trie comparable a ) -> List ( ( List comparable, Trie comparable a ), Bool )
wildcardStepr ( keyAccum, Trie _ dict ) =
    Dict.foldr
        (\k ((Trie maybeValue _) as innerTrie) stack ->
            ( ( k :: keyAccum, innerTrie ), isJust maybeValue )
                :: stack
        )
        []
        dict


{-| Performs a fold over all goals of a search over Tries, until the searh is complete.
-}
foldSearchGoals : (List comparable -> a -> b -> b) -> b -> Search.SearchResult ( List comparable, Trie comparable a ) -> b
foldSearchGoals fn accum search =
    case Search.nextGoal search of
        Search.Complete ->
            accum

        Search.Goal ( key, Trie maybeValue _ ) searchFn ->
            case maybeValue of
                Nothing ->
                    foldSearchGoals fn accum (searchFn ())

                Just value ->
                    foldSearchGoals fn (fn key value accum) (searchFn ())

        Search.Ongoing _ searchFn ->
            foldSearchGoals fn accum (searchFn ())
