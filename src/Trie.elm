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
update key fn trie =
    updateInner (String.toList key) fn trie


updateInner : List Char -> (Maybe a -> Maybe a) -> Trie a -> Trie a
updateInner key fn trie =
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
                                        singletonInner tail fnVal |> Just

                            Just existingTrie ->
                                updateInner tail fn existingTrie |> Just
                    )
                    rem
                )


remove : String -> Trie a -> Trie a
remove key trie =
    removeInner (String.toList key) trie


removeInner : List Char -> Trie a -> Trie a
removeInner key trie =
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
                                removeInner tail existingTrie |> Just
                    )
                    rem
                )


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
map _ _ =
    Debug.todo "map"


foldl : (String -> a -> b -> b) -> b -> Trie a -> b
foldl _ _ _ =
    Debug.todo "foldl"


walkl : (List Char -> Maybe a -> b -> b) -> b -> Trie a -> b
walkl _ _ _ =
    Debug.todo "walkl"


foldr : (String -> a -> b -> b) -> b -> Trie a -> b
foldr fn accum trie =
    walkr (\chars -> fn <| String.fromList (List.reverse chars)) accum trie


walkr : (List Char -> a -> b -> b) -> b -> Trie a -> b
walkr fn accum ((Trie maybeValue _) as trie) =
    Search.depthFirst { step = step, cost = \_ -> 1.0 }
        [ ( ( [], trie )
          , case maybeValue of
                Nothing ->
                    False

                Just _ ->
                    True
          )
        ]
        |> foldGoals fn accum


step : ( List Char, Trie a ) -> List ( ( List Char, Trie a ), Bool )
step ( keyAccum, Trie _ dict ) =
    Dict.foldr
        (\k ((Trie maybeValue _) as innerTrie) stack ->
            ( ( k :: keyAccum, innerTrie )
            , case maybeValue of
                Nothing ->
                    False

                Just _ ->
                    True
            )
                :: stack
        )
        []
        dict


foldGoals : (List Char -> a -> b -> b) -> b -> Search.SearchResult ( List Char, Trie a ) -> b
foldGoals fn accum search =
    case Search.nextGoal search of
        Search.Complete ->
            accum

        Search.Goal ( key, Trie maybeValue _ ) searchFn ->
            case maybeValue of
                Nothing ->
                    foldGoals fn accum (searchFn ())

                Just value ->
                    foldGoals fn (fn key value accum) (searchFn ())

        Search.Ongoing _ searchFn ->
            foldGoals fn accum (searchFn ())


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
