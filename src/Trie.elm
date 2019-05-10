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


type Trie a
    = Trie (Maybe a) (Dict Char (Trie a))


empty : Trie a
empty =
    Trie Nothing Dict.empty


singleton : String -> a -> Trie a
singleton key val =
    singletonInner (String.toList key) val


singletonInner : List Char -> a -> Trie a
singletonInner key val =
    case key of
        [] ->
            Trie (Just val) Dict.empty

        head :: tail ->
            Trie Nothing (Dict.singleton head (singletonInner tail val))


insert : String -> a -> Trie a -> Trie a
insert key val trie =
    insertInner (String.toList key) val trie


insertInner : List Char -> a -> Trie a -> Trie a
insertInner key val trie =
    case ( key, trie ) of
        ( [], Trie _ rem ) ->
            Trie (Just val) rem

        ( head :: tail, Trie maybeSomeVal rem ) ->
            Trie maybeSomeVal
                (Dict.update head
                    (\maybeTrie ->
                        case maybeTrie of
                            Nothing ->
                                singletonInner tail val |> Just

                            Just existingTrie ->
                                insertInner tail val existingTrie |> Just
                    )
                    rem
                )


update : String -> (Maybe a -> Maybe a) -> Trie a -> Trie a
update _ _ trie =
    trie


remove : String -> Trie a -> Trie a
remove _ _ =
    Debug.todo "remove"


isEmpty : Trie a -> Bool
isEmpty trie =
    Debug.todo "isEmpty"


member : String -> Trie a -> Bool
member _ _ =
    Debug.todo "member"


get : String -> Trie a -> Maybe a
get key trie =
    getInner (String.toList key) trie


getInner : List Char -> Trie a -> Maybe a
getInner key trie =
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
                    getInner tail subTrie


size : Trie a -> Int
size (Trie maybeVal dict) =
    case maybeVal of
        Nothing ->
            Dict.foldl (\k v accum -> accum + size v) 0 dict

        Just _ ->
            Dict.foldl (\k v accum -> accum + size v) 1 dict


keys : Trie a -> List String
keys _ =
    Debug.todo "keys"


values : Trie a -> List a
values _ =
    Debug.todo "values"


toList : Trie a -> List ( String, a )
toList _ =
    Debug.todo "toList"


fromList : List ( String, a ) -> Trie a
fromList _ =
    Debug.todo "fromList"


map : (String -> a -> b) -> Trie a -> Trie b
map _ _ =
    Debug.todo "map"


foldl : (String -> a -> b -> b) -> b -> Trie a -> b
foldl _ _ _ =
    Debug.todo "foldl"


foldr : (String -> a -> b -> b) -> b -> Trie a -> b
foldr _ _ _ =
    Debug.todo "foldr"


filter : (String -> a -> Bool) -> Trie a -> Trie a
filter _ _ =
    Debug.todo "filter"


partition : (String -> a -> Bool) -> Trie a -> ( Trie a, Trie a )
partition _ _ =
    Debug.todo "partition"


union : Trie a -> Trie a -> Trie a
union _ _ =
    Debug.todo "union"


intersect : Trie a -> Trie a -> Trie a
intersect _ _ =
    Debug.todo "intersect"


diff : Trie a -> Trie b -> Trie a
diff _ _ =
    Debug.todo "diff"


merge :
    (String -> a -> result -> result)
    -> (String -> a -> b -> result -> result)
    -> (String -> b -> result -> result)
    -> Trie a
    -> Trie b
    -> result
    -> result
merge _ _ _ _ _ _ =
    Debug.todo "merge"


expand : String -> Trie a -> List String
expand _ _ =
    Debug.todo "expand"


matches : String -> Trie a -> Bool
matches _ _ =
    Debug.todo "matches"


subtrie : String -> Trie a -> Maybe (Trie a)
subtrie _ _ =
    Debug.todo "subtrie"



-- Ideas
-- expandIgnoreCase : String -> Trie a -> List String
-- matchesIgnoreCase : String -> Trie a -> Bool
-- match : (List Char -> Char -> a -> Bool) -> Trie a -> Bool
-- match _ _ =
--     Debug.todo "map"
