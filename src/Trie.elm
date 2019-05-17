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


singleton : String -> a -> Trie Char a
singleton key val =
    singletonInner (String.toList key) val


insert : String -> a -> Trie Char a -> Trie Char a
insert key val trie =
    insertInner (String.toList key) val trie


update : String -> (Maybe a -> Maybe a) -> Trie Char a -> Trie Char a
update key fn trie =
    updateInner (String.toList key) fn trie


remove : String -> Trie Char a -> Trie Char a
remove key trie =
    removeInner (String.toList key) trie


isEmpty : Trie Char a -> Bool
isEmpty trie =
    size trie == 0


member : String -> Trie Char a -> Bool
member key trie =
    case subtrie key trie of
        Nothing ->
            False

        Just (Trie maybeValue _) ->
            isJust maybeValue


get : String -> Trie Char a -> Maybe a
get key trie =
    getInner (String.toList key) trie


size : Trie Char a -> Int
size trie =
    foldr (\_ _ accum -> accum + 1) 0 trie


keys : Trie Char a -> List String
keys trie =
    foldr (\key value keyList -> key :: keyList) [] trie


values : Trie Char a -> List a
values trie =
    foldr (\key value valueList -> value :: valueList) [] trie


toList : Trie Char a -> List ( String, a )
toList trie =
    foldr (\key value list -> ( key, value ) :: list) [] trie


fromList : List ( String, a ) -> Trie Char a
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs


map : (String -> a -> b) -> Trie Char a -> Trie Char b
map fn trie =
    mapInner (\chars -> fn <| String.fromList (List.reverse chars)) [] trie


foldl : (String -> a -> b -> b) -> b -> Trie Char a -> b
foldl fn accum trie =
    foldlInner (\chars -> fn <| String.fromList (List.reverse chars)) accum trie


foldr : (String -> a -> b -> b) -> b -> Trie Char a -> b
foldr fn accum trie =
    foldrInner (\chars -> fn <| String.fromList (List.reverse chars)) accum trie


filter : (String -> a -> Bool) -> Trie Char a -> Trie Char a
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


partition : (String -> a -> Bool) -> Trie Char a -> ( Trie Char a, Trie Char a )
partition isGood trie =
    let
        add key value ( t1, t2 ) =
            if isGood key value then
                ( insert key value t1, t2 )

            else
                ( t1, insert key value t2 )
    in
    foldl add ( empty, empty ) trie


union : Trie Char a -> Trie Char a -> Trie Char a
union t1 t2 =
    foldl insert t2 t1


intersect : Trie Char a -> Trie Char a -> Trie Char a
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


diff : Trie Char a -> Trie Char b -> Trie Char a
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


merge :
    (String -> a -> result -> result)
    -> (String -> a -> b -> result -> result)
    -> (String -> b -> result -> result)
    -> Trie Char a
    -> Trie Char b
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


expand : String -> Trie Char a -> List String
expand key trie =
    case subtrie key trie of
        Nothing ->
            []

        Just innerTrie ->
            foldr (\innerKey _ accum -> (key ++ innerKey) :: accum) [] innerTrie


matches : String -> Trie Char a -> Bool
matches key trie =
    case subtrie key trie of
        Nothing ->
            False

        Just (Trie maybeValue _) ->
            isJust maybeValue


subtrie : String -> Trie Char a -> Maybe (Trie Char a)
subtrie key trie =
    subtrieInner (String.toList key) trie



-- Inner functions


singletonInner : List comparable -> a -> Trie comparable a
singletonInner key val =
    case key of
        [] ->
            Trie (Just val) Dict.empty

        head :: tail ->
            Trie Nothing (Dict.singleton head (singletonInner tail val))


insertInner : List comparable -> a -> Trie comparable a -> Trie comparable a
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


updateInner : List comparable -> (Maybe a -> Maybe a) -> Trie comparable a -> Trie comparable a
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


removeInner : List comparable -> Trie comparable a -> Trie comparable a
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


getInner : List comparable -> Trie comparable a -> Maybe a
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


mapInner : (List comparable -> a -> b) -> List comparable -> Trie comparable a -> Trie comparable b
mapInner fn keyAccum ((Trie maybeValue dict) as trie) =
    case maybeValue of
        Nothing ->
            Trie Nothing (Dict.map (\key innerTrie -> mapInner fn (key :: keyAccum) innerTrie) dict)

        Just value ->
            Trie (Just <| fn keyAccum value) (Dict.map (\key innerTrie -> mapInner fn (key :: keyAccum) innerTrie) dict)


foldlInner : (List comparable -> a -> b -> b) -> b -> Trie comparable a -> b
foldlInner fn accum ((Trie maybeValue _) as trie) =
    Search.depthFirst { step = wildcardStepl, cost = \_ -> 1.0 }
        [ ( ( [], trie ), isJust maybeValue ) ]
        |> foldSearchGoals fn accum


foldrInner : (List comparable -> a -> b -> b) -> b -> Trie comparable a -> b
foldrInner fn accum ((Trie maybeValue _) as trie) =
    Search.depthFirst { step = wildcardStepr, cost = \_ -> 1.0 }
        [ ( ( [], trie ), isJust maybeValue ) ]
        |> foldSearchGoals fn accum


subtrieInner : List comparable -> Trie comparable a -> Maybe (Trie comparable a)
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
