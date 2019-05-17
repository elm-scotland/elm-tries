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

import Trie


type alias Trie a =
    Trie.Trie a


empty : Trie a
empty =
    Trie Nothing Dict.empty


singleton : String -> a -> Trie a
singleton key val =
    singletonInner (String.toList key) val


insert : String -> a -> Trie a -> Trie a
insert key val trie =
    insertInner (String.toList key) val trie


update : String -> (Maybe a -> Maybe a) -> Trie a -> Trie a
update key fn trie =
    updateInner (String.toList key) fn trie


remove : String -> Trie a -> Trie a
remove key trie =
    removeInner (String.toList key) trie


isEmpty : Trie a -> Bool
isEmpty trie =
    size trie == 0


member : String -> Trie a -> Bool
member key trie =
    case subtrie key trie of
        Nothing ->
            False

        Just (Trie maybeValue _) ->
            isJust maybeValue


get : String -> Trie a -> Maybe a
get key trie =
    getInner (String.toList key) trie


size : Trie a -> Int
size trie =
    foldr (\_ _ accum -> accum + 1) 0 trie


keys : Trie a -> List String
keys trie =
    foldr (\key value keyList -> key :: keyList) [] trie


values : Trie a -> List a
values trie =
    foldr (\key value valueList -> value :: valueList) [] trie


toList : Trie a -> List ( String, a )
toList trie =
    foldr (\key value list -> ( key, value ) :: list) [] trie


fromList : List ( String, a ) -> Trie a
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs


map : (String -> a -> b) -> Trie a -> Trie b
map fn trie =
    mapInner (\chars -> fn <| String.fromList (List.reverse chars)) [] trie


foldl : (String -> a -> b -> b) -> b -> Trie a -> b
foldl fn accum trie =
    foldlInner (\chars -> fn <| String.fromList (List.reverse chars)) accum trie


foldr : (String -> a -> b -> b) -> b -> Trie a -> b
foldr fn accum trie =
    foldrInner (\chars -> fn <| String.fromList (List.reverse chars)) accum trie


filter : (String -> a -> Bool) -> Trie a -> Trie a
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


partition : (String -> a -> Bool) -> Trie a -> ( Trie a, Trie a )
partition isGood trie =
    let
        add key value ( t1, t2 ) =
            if isGood key value then
                ( insert key value t1, t2 )

            else
                ( t1, insert key value t2 )
    in
    foldl add ( empty, empty ) trie


union : Trie a -> Trie a -> Trie a
union t1 t2 =
    foldl insert t2 t1


intersect : Trie a -> Trie a -> Trie a
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


diff : Trie a -> Trie b -> Trie a
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


merge :
    (String -> a -> result -> result)
    -> (String -> a -> b -> result -> result)
    -> (String -> b -> result -> result)
    -> Trie a
    -> Trie b
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


expand : String -> Trie a -> List String
expand key trie =
    case subtrie key trie of
        Nothing ->
            []

        Just innerTrie ->
            foldr (\innerKey _ accum -> (key ++ innerKey) :: accum) [] innerTrie


matches : String -> Trie a -> Bool
matches key trie =
    case subtrie key trie of
        Nothing ->
            False

        Just (Trie maybeValue _) ->
            isJust maybeValue


subtrie : String -> Trie a -> Maybe (Trie a)
subtrie key trie =
    subtrieInner (String.toList key) trie
