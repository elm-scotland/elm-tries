module Trie exposing (Trie(..), diff, empty, expand, filter, foldl, foldr, fromList, get, insert, intersect, isEmpty, keys, map, matches, member, merge, partition, remove, singleton, size, subtrie, toList, union, update, values)

import Dict exposing (Dict)


type Trie a
    = Trie (Maybe a) (Dict Char (Trie a))


empty : Trie a
empty =
    Trie Nothing Dict.empty


singleton : String -> a -> Trie a
singleton key val =
    let
        maybeUncons =
            String.uncons key
    in
    case maybeUncons of
        Nothing ->
            Trie (Just val) Dict.empty

        Just ( head, tail ) ->
            Trie Nothing (Dict.singleton head (singleton tail val))


insert : String -> a -> Trie a -> Trie a
insert key val trie =
    let
        maybeUncons =
            String.uncons key
    in
    case ( maybeUncons, trie ) of
        ( Nothing, Trie _ rem ) ->
            Trie (Just val) rem

        ( Just ( head, tail ), Trie maybeSomeVal rem ) ->
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


update : String -> (Maybe a -> Maybe a) -> Trie a -> Trie a
update _ _ _ =
    Debug.todo "update"


remove : String -> Trie a -> Trie a
remove _ _ =
    Debug.todo "remove"


isEmpty : Trie a -> Bool
isEmpty trie =
    case trie of
        Trie Nothing dict ->
            Dict.isEmpty dict

        _ ->
            False


member : String -> Trie a -> Bool
member _ _ =
    Debug.todo "member"


get : String -> Trie a -> Maybe a
get key trie =
    let
        maybeUncons =
            String.uncons key
    in
    case ( maybeUncons, trie ) of
        ( Nothing, Trie maybeSomeVal _ ) ->
            maybeSomeVal

        ( Just ( head, tail ), Trie _ rem ) ->
            let
                maybeTrie =
                    Dict.get head rem
            in
            case maybeTrie of
                Nothing ->
                    Nothing

                Just subTrie ->
                    get tail subTrie


size : Trie a -> Int
size _ =
    -- cache this on inserts
    Debug.todo "size"


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
