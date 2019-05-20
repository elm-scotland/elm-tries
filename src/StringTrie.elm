module StringTrie exposing
    ( Trie
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
import Trie


type alias Trie a =
    Trie.Trie Char a


empty : Trie a
empty =
    Trie.empty


singleton : String -> a -> Trie a
singleton key val =
    Trie.singleton (String.toList key) val


insert : String -> a -> Trie a -> Trie a
insert key val trie =
    Trie.insert (String.toList key) val trie


update : String -> (Maybe a -> Maybe a) -> Trie a -> Trie a
update key fn trie =
    Trie.update (String.toList key) fn trie


remove : String -> Trie a -> Trie a
remove key trie =
    Trie.remove (String.toList key) trie


isEmpty : Trie a -> Bool
isEmpty trie =
    Trie.isEmpty trie


member : String -> Trie a -> Bool
member key trie =
    Trie.member (String.toList key) trie


get : String -> Trie a -> Maybe a
get key trie =
    Trie.get (String.toList key) trie


size : Trie a -> Int
size trie =
    Trie.size trie


keys : Trie a -> List String
keys trie =
    Trie.keys trie
        |> List.map String.fromList


values : Trie a -> List a
values trie =
    Trie.values trie


toList : Trie a -> List ( String, a )
toList trie =
    Trie.toList trie
        |> List.map (Tuple.mapFirst String.fromList)


fromList : List ( String, a ) -> Trie a
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs


map : (String -> a -> b) -> Trie a -> Trie b
map fn trie =
    Trie.map (\chars -> fn <| String.fromList chars) trie


foldl : (String -> a -> b -> b) -> b -> Trie a -> b
foldl fn accum trie =
    Trie.foldl (\chars -> fn <| String.fromList chars) accum trie


foldr : (String -> a -> b -> b) -> b -> Trie a -> b
foldr fn accum trie =
    Trie.foldr (\chars -> fn <| String.fromList chars) accum trie


filter : (String -> a -> Bool) -> Trie a -> Trie a
filter isGood trie =
    Trie.filter (\chars -> isGood <| String.fromList chars) trie


partition : (String -> a -> Bool) -> Trie a -> ( Trie a, Trie a )
partition isGood trie =
    Trie.partition (\chars -> isGood <| String.fromList chars) trie


union : Trie a -> Trie a -> Trie a
union t1 t2 =
    Trie.union t1 t2


intersect : Trie a -> Trie a -> Trie a
intersect t1 t2 =
    Trie.intersect t1 t2


diff : Trie a -> Trie b -> Trie a
diff t1 t2 =
    Trie.diff t1 t2


merge :
    (String -> a -> result -> result)
    -> (String -> a -> b -> result -> result)
    -> (String -> b -> result -> result)
    -> Trie a
    -> Trie b
    -> result
    -> result
merge leftStep bothStep rightStep leftTrie rightTrie initialResult =
    Trie.merge
        (\chars -> leftStep <| String.fromList chars)
        (\chars -> bothStep <| String.fromList chars)
        (\chars -> rightStep <| String.fromList chars)
        leftTrie
        rightTrie
        initialResult


expand : String -> Trie a -> List String
expand key trie =
    Trie.expand (String.toList key) trie
        |> List.map String.fromList


matches : String -> Trie a -> Bool
matches key trie =
    Trie.matches (String.toList key) trie


subtrie : String -> Trie a -> Maybe (Trie a)
subtrie key trie =
    Trie.subtrie (String.toList key) trie
