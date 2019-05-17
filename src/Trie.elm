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
-- expandIgnoreCase : String -> Trie a -> List String
-- matchesIgnoreCase : String -> Trie a -> Bool
-- match : (List Char -> Char -> a -> Bool) -> Trie a -> Bool
-- match _ _ =
--     Debug.todo "map"


type Trie a
    = Trie (Maybe a) (Dict Char (Trie a))


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



-- Inner functions


singletonInner : List Char -> a -> Trie a
singletonInner key val =
    case key of
        [] ->
            Trie (Just val) Dict.empty

        head :: tail ->
            Trie Nothing (Dict.singleton head (singletonInner tail val))


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


mapInner : (List Char -> a -> b) -> List Char -> Trie a -> Trie b
mapInner fn keyAccum ((Trie maybeValue dict) as trie) =
    case maybeValue of
        Nothing ->
            Trie Nothing (Dict.map (\key innerTrie -> mapInner fn (key :: keyAccum) innerTrie) dict)

        Just value ->
            Trie (Just <| fn keyAccum value) (Dict.map (\key innerTrie -> mapInner fn (key :: keyAccum) innerTrie) dict)


foldlInner : (List Char -> a -> b -> b) -> b -> Trie a -> b
foldlInner fn accum ((Trie maybeValue _) as trie) =
    Search.depthFirst { step = wildcardStepl, cost = \_ -> 1.0 }
        [ ( ( [], trie ), isJust maybeValue ) ]
        |> foldSearchGoals fn accum


foldrInner : (List Char -> a -> b -> b) -> b -> Trie a -> b
foldrInner fn accum ((Trie maybeValue _) as trie) =
    Search.depthFirst { step = wildcardStepr, cost = \_ -> 1.0 }
        [ ( ( [], trie ), isJust maybeValue ) ]
        |> foldSearchGoals fn accum


subtrieInner : List Char -> Trie a -> Maybe (Trie a)
subtrieInner key ((Trie maybeValue dict) as trie) =
    case key of
        [] ->
            Just trie

        k :: ks ->
            case Dict.get k dict of
                Nothing ->
                    Nothing

                Just innerTrie ->
                    subtrieInner ks innerTrie



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
wildcardStepl : ( List Char, Trie a ) -> List ( ( List Char, Trie a ), Bool )
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
wildcardStepr : ( List Char, Trie a ) -> List ( ( List Char, Trie a ), Bool )
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
foldSearchGoals : (List Char -> a -> b -> b) -> b -> Search.SearchResult ( List Char, Trie a ) -> b
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
