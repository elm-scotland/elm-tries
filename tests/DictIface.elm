module DictIface exposing
    ( IDict
    , emptyContainsNoVal
    , emptyInsertStringContainsVal
    , emptyIsEmpty
    , listOfNumsDoubledAllEven
    , listOfValsAllKeysMembers
    , listOfValsContainsAllVals
    , listOfValsFoldlAllKeys
    , listOfValsFoldlIncreasing
    , listOfValsFoldrAllKeys
    , listOfValsFoldrDecreasing
    , listOfValsListsAllKeys
    , listOfValsListsAllValues
    , listOfValsRemovedContainsNone
    , listOfValsReportsSizeOk
    , nonEmptyIsNotEmpty
    , singletonContainsVal
    , singletonEmptyStringContainsVal
    )

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Set exposing (Set)
import Test exposing (Test, fuzz)


type alias IDict comparable v dict b dictb result x =
    { x
        | empty : dict
        , singleton : comparable -> v -> dict
        , insert : comparable -> v -> dict -> dict
        , update : comparable -> (Maybe v -> Maybe v) -> dict -> dict
        , remove : comparable -> dict -> dict
        , isEmpty : dict -> Bool
        , member : comparable -> dict -> Bool
        , get : comparable -> dict -> Maybe v
        , size : dict -> Int
        , keys : dict -> List comparable
        , values : dict -> List v
        , toList : dict -> List ( comparable, v )
        , fromList : List ( comparable, v ) -> dict
        , map : (comparable -> v -> b) -> dict -> dictb
        , foldl : (comparable -> v -> b -> b) -> b -> dict -> b
        , foldr : (comparable -> v -> b -> b) -> b -> dict -> b
        , filter : (comparable -> v -> Bool) -> dict -> dict
        , partition : (comparable -> v -> Bool) -> dict -> ( dict, dict )
        , union : dict -> dict -> dict
        , intersect : dict -> dict -> dict
        , diff : dict -> dictb -> dict
        , merge :
            (comparable -> v -> result -> result)
            -> (comparable -> v -> b -> result -> result)
            -> (comparable -> b -> result -> result)
            -> dict
            -> dictb
            -> result
            -> result
    }



-- Key type specific tests.


singletonEmptyStringContainsVal : String -> String -> Fuzzer String -> IDict String String dict b dictb result x -> Test
singletonEmptyStringContainsVal fuzzName implName fuzzer dictImpl =
    let
        test val =
            dictImpl.singleton "" val |> dictImpl.get "" |> Expect.equal (Just val)
    in
    fuzz fuzzer
        ("Creates singleton " ++ implName ++ " with the empty string as key (" ++ fuzzName ++ ").")
        test


listOfNumsDoubledAllEven : String -> String -> Fuzzer (List Int) -> IDict String Int dict b dictb result x -> Test
listOfNumsDoubledAllEven fuzzName implName fuzzer dictImpl =
    let
        doubleOdd keys trie =
            List.foldl
                (\key accum ->
                    dictImpl.update (String.fromInt key)
                        (\maybeVal ->
                            case maybeVal of
                                Nothing ->
                                    Nothing

                                Just num ->
                                    Just (num * 2)
                        )
                        accum
                )
                trie
                keys

        expectJustEven maybeVal =
            case maybeVal of
                Nothing ->
                    Expect.fail "nothing"

                Just val ->
                    if modBy 2 val == 0 then
                        Expect.pass

                    else
                        Expect.fail "not even"

        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert (String.fromInt val) val trie) dictImpl.empty vals
                        |> doubleOdd vals
                        |> Expect.all (List.map (\val trie -> dictImpl.get (String.fromInt val) trie |> expectJustEven) vals)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of numbers, then updates to double all numbers that are odd, and checks all vals are even (" ++ fuzzName ++ ").")
        test



-- Generic comparable key tests.


emptyIsEmpty : String -> IDict comparable a dict b dictb result x -> Test
emptyIsEmpty implName dictImpl =
    let
        test () =
            dictImpl.empty |> dictImpl.isEmpty |> Expect.true "not empty"
    in
    Test.test "Check empty trie reports isEmpty." test


emptyContainsNoVal : String -> String -> Fuzzer comparable -> IDict comparable comparable dict b dictb result x -> Test
emptyContainsNoVal fuzzName implName fuzzer dictImpl =
    let
        test val =
            dictImpl.empty |> dictImpl.get val |> Expect.equal Nothing
    in
    fuzz fuzzer ("Checks an empty " ++ implName ++ " does not get any keys (" ++ fuzzName ++ ").") test


nonEmptyIsNotEmpty : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
nonEmptyIsNotEmpty fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> dictImpl.isEmpty
                        |> Expect.false "empty"
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " and checks if it is non-empty that it reports it is not empty (" ++ fuzzName ++ ").")
        test


singletonContainsVal : String -> String -> Fuzzer comparable -> IDict comparable comparable dict b dictb result x -> Test
singletonContainsVal fuzzName implName fuzzer dictImpl =
    let
        test val =
            dictImpl.singleton val val |> dictImpl.get val |> Expect.equal (Just val)
    in
    fuzz fuzzer
        ("Creates singleton " ++ implName ++ " (" ++ fuzzName ++ ").")
        test


emptyInsertStringContainsVal : String -> String -> Fuzzer comparable -> IDict comparable comparable dict b dictb result x -> Test
emptyInsertStringContainsVal fuzzName implName fuzzer dictImpl =
    let
        test val =
            dictImpl.empty |> dictImpl.insert val val |> dictImpl.get val |> Expect.equal (Just val)
    in
    fuzz fuzzer
        ("Creates " ++ implName ++ " by inserting to empty (" ++ fuzzName ++ ").")
        test


listOfValsContainsAllVals : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
listOfValsContainsAllVals fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> Expect.all (List.map (\val trie -> dictImpl.get val trie |> Expect.equal (Just val)) vals)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals and ensures it contains all of them (" ++ fuzzName ++ ").")
        test


listOfValsReportsSizeOk : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
listOfValsReportsSizeOk fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal 0 (dictImpl.size dictImpl.empty)

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> dictImpl.size
                        |> Expect.equal (Set.size (Set.fromList vals))
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " trie with a list of vals and checks it has the correct size (" ++ fuzzName ++ ").")
        test


listOfValsAllKeysMembers : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
listOfValsAllKeysMembers fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> (\trie -> Expect.all (List.map (\val list -> dictImpl.member val trie |> Expect.true "not member of trie") vals) trie)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals and ensures all keys are members (" ++ fuzzName ++ ").")
        test


listOfValsRemovedContainsNone : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
listOfValsRemovedContainsNone fuzzName implName fuzzer dictImpl =
    let
        removeAll keys trie =
            List.foldl (\key accum -> dictImpl.remove key accum) trie keys

        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> removeAll vals
                        |> Expect.all (List.map (\val trie -> dictImpl.get val trie |> Expect.equal Nothing) vals)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals, removes all, and checks none are present (" ++ fuzzName ++ ").")
        test


listOfValsListsAllKeys : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
listOfValsListsAllKeys fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> dictImpl.keys
                        |> Expect.all (List.map (\val list -> List.member val list |> Expect.true "not member of keys") vals)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals and ensures lists all of them as keys (" ++ fuzzName ++ ").")
        test


listOfValsListsAllValues : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
listOfValsListsAllValues fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> dictImpl.values
                        |> Expect.all (List.map (\val list -> List.member val list |> Expect.true "not member of keys") vals)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals and ensures lists all of them as values (" ++ fuzzName ++ ").")
        test


listOfValsFoldlAllKeys : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict (Set comparable) dictb result x -> Test
listOfValsFoldlAllKeys fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> dictImpl.foldl (\k _ accum -> Set.insert k accum) Set.empty
                        |> Expect.all (List.map (\val list -> Set.member val list |> Expect.true "not member of foldl keys") vals)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals and folds it left, checking all keys are gathered in the fold (" ++ fuzzName ++ ").")
        test


listOfValsFoldrAllKeys : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict (Set comparable) dictb result x -> Test
listOfValsFoldrAllKeys fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> dictImpl.foldr (\k _ accum -> Set.insert k accum) Set.empty
                        |> Expect.all (List.map (\val list -> Set.member val list |> Expect.true "not member of foldl keys") vals)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals and folds it right, checking all keys are gathered in the fold (" ++ fuzzName ++ ").")
        test


listOfValsFoldlIncreasing :
    String
    -> String
    -> Fuzzer (List comparable)
    -> IDict comparable comparable dict ( Maybe comparable, Maybe ( comparable, comparable ) ) dictb result x
    -> Test
listOfValsFoldlIncreasing fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> dictImpl.foldl
                            (\k _ ( maybekCompare, _ ) ->
                                case maybekCompare of
                                    Nothing ->
                                        ( Just k, Nothing )

                                    Just kCompare ->
                                        ( Just k
                                        , if k >= kCompare then
                                            Nothing

                                          else
                                            Just ( k, kCompare )
                                        )
                            )
                            ( Nothing, Nothing )
                        |> Tuple.second
                        |> Maybe.map (\( k, kCompare ) -> Expect.atLeast kCompare k)
                        |> Maybe.withDefault Expect.pass
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals and folds it left, checking all keys are explored in order from least to most (" ++ fuzzName ++ ").")
        test


listOfValsFoldrDecreasing :
    String
    -> String
    -> Fuzzer (List comparable)
    -> IDict comparable comparable dict ( Maybe comparable, Maybe ( comparable, comparable ) ) dictb result x
    -> Test
listOfValsFoldrDecreasing fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> dictImpl.foldr
                            (\k _ ( maybekCompare, _ ) ->
                                case maybekCompare of
                                    Nothing ->
                                        ( Just k, Nothing )

                                    Just kCompare ->
                                        ( Just k
                                        , if k <= kCompare then
                                            Nothing

                                          else
                                            Just ( k, kCompare )
                                        )
                            )
                            ( Nothing, Nothing )
                        |> Tuple.second
                        |> Maybe.map (\( k, kCompare ) -> Expect.atMost kCompare k)
                        |> Maybe.withDefault Expect.pass
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals and folds it right, checking all keys are explored in order from most to least (" ++ fuzzName ++ ").")
        test
