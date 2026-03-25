module DictIface exposing
    ( IDict
    , diffRemovesSecondKeys
    , emptyContainsNoVal
    , emptyInsertStringContainsVal
    , emptyIsEmpty
    , filterFalseRemovesAll
    , filterTrueKeepsAll
    , fromListToListRoundtrip
    , insertReplacesValue
    , intersectOnlyCommonKeys
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
    , mapPreservesKeys
    , nonEmptyIsNotEmpty
    , partitionIsExhaustive
    , removeNonExistentIsNoop
    , singletonContainsVal
    , singletonEmptyStringContainsVal
    , unionContainsBothKeys
    , unionWithEmptyIsIdentity
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
            dictImpl.empty |> dictImpl.isEmpty |> Expect.equal True
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
                        |> Expect.equal False
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
                        |> (\trie -> Expect.all (List.map (\val list -> dictImpl.member val trie |> Expect.equal True) vals) trie)
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
                        |> Expect.all (List.map (\val list -> List.member val list |> Expect.equal True) vals)
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
                        |> Expect.all (List.map (\val list -> List.member val list |> Expect.equal True) vals)
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
                        |> Expect.all (List.map (\val list -> Set.member val list |> Expect.equal True) vals)
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
                        |> Expect.all (List.map (\val list -> Set.member val list |> Expect.equal True) vals)
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


mapPreservesKeys : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict comparable dict result x -> Test
mapPreservesKeys fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        trie =
                            List.foldl (\val t -> dictImpl.insert val val t) dictImpl.empty vals

                        mapped =
                            dictImpl.map (\_ v -> v) trie
                    in
                    dictImpl.keys mapped
                        |> (\mappedKeys ->
                                Expect.all
                                    (List.map (\val ks -> List.member val ks |> Expect.equal True) vals)
                                    mappedKeys
                           )
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals, maps identity, and checks all keys are preserved (" ++ fuzzName ++ ").")
        test


filterTrueKeepsAll : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
filterTrueKeepsAll fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        trie =
                            List.foldl (\val t -> dictImpl.insert val val t) dictImpl.empty vals
                    in
                    dictImpl.filter (\_ _ -> True) trie
                        |> dictImpl.size
                        |> Expect.equal (dictImpl.size trie)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals, filters with always True, and checks size is preserved (" ++ fuzzName ++ ").")
        test


filterFalseRemovesAll : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
filterFalseRemovesAll fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        trie =
                            List.foldl (\val t -> dictImpl.insert val val t) dictImpl.empty vals
                    in
                    dictImpl.filter (\_ _ -> False) trie
                        |> dictImpl.isEmpty
                        |> Expect.equal True
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals, filters with always False, and checks result is empty (" ++ fuzzName ++ ").")
        test


partitionIsExhaustive : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
partitionIsExhaustive fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        trie =
                            List.foldl (\val t -> dictImpl.insert val val t) dictImpl.empty vals

                        uniqueKeys =
                            dictImpl.keys trie

                        halfKeys =
                            Set.fromList (List.take (List.length uniqueKeys // 2) uniqueKeys)

                        ( t1, t2 ) =
                            dictImpl.partition (\k _ -> Set.member k halfKeys) trie
                    in
                    (dictImpl.size t1 + dictImpl.size t2)
                        |> Expect.equal (dictImpl.size trie)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals, partitions it, and checks both halves sum to original size (" ++ fuzzName ++ ").")
        test


unionWithEmptyIsIdentity : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
unionWithEmptyIsIdentity fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        trie =
                            List.foldl (\val t -> dictImpl.insert val val t) dictImpl.empty vals
                    in
                    dictImpl.union trie dictImpl.empty
                        |> dictImpl.size
                        |> Expect.equal (dictImpl.size trie)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " and checks union with empty preserves size (" ++ fuzzName ++ ").")
        test


unionContainsBothKeys : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
unionContainsBothKeys fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        half =
                            List.length vals // 2

                        vals1 =
                            List.take half vals

                        vals2 =
                            List.drop half vals

                        t1 =
                            List.foldl (\val t -> dictImpl.insert val val t) dictImpl.empty vals1

                        t2 =
                            List.foldl (\val t -> dictImpl.insert val val t) dictImpl.empty vals2

                        combined =
                            dictImpl.union t1 t2
                    in
                    List.all (\val -> dictImpl.member val combined) vals
                        |> Expect.equal True
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " by unioning two halves and checks all keys are present (" ++ fuzzName ++ ").")
        test


intersectOnlyCommonKeys : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
intersectOnlyCommonKeys fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        t1 =
                            List.foldl (\val t -> dictImpl.insert val val t) dictImpl.empty vals

                        subsetVals =
                            List.take (List.length vals // 2) vals

                        t2 =
                            List.foldl (\val t -> dictImpl.insert val val t) dictImpl.empty subsetVals

                        inter =
                            dictImpl.intersect t1 t2

                        interKeys =
                            dictImpl.keys inter
                    in
                    List.all (\k -> dictImpl.member k t1 && dictImpl.member k t2) interKeys
                        |> Expect.equal True
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " and checks intersect keys are members of both inputs (" ++ fuzzName ++ ").")
        test


diffRemovesSecondKeys : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict comparable dict result x -> Test
diffRemovesSecondKeys fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        t1 =
                            List.foldl (\val t -> dictImpl.insert val val t) dictImpl.empty vals

                        subsetVals =
                            List.take (List.length vals // 2) vals

                        t2 =
                            List.foldl (\val t -> dictImpl.insert val val t) dictImpl.empty subsetVals

                        d =
                            dictImpl.diff t1 t2
                    in
                    List.all (\val -> not (dictImpl.member val d)) subsetVals
                        |> Expect.equal True
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " and checks diff removes keys from the second input (" ++ fuzzName ++ ").")
        test


fromListToListRoundtrip : String -> String -> Fuzzer (List comparable) -> IDict comparable comparable dict b dictb result x -> Test
fromListToListRoundtrip fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        pairs =
                            List.map (\v -> ( v, v )) vals

                        trie =
                            dictImpl.fromList pairs

                        resultKeys =
                            dictImpl.toList trie |> List.map Tuple.first |> Set.fromList

                        expectedKeys =
                            Set.fromList vals
                    in
                    resultKeys |> Expect.equal expectedKeys
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with fromList and checks toList contains all keys (" ++ fuzzName ++ ").")
        test


insertReplacesValue : String -> String -> Fuzzer comparable -> IDict comparable comparable dict b dictb result x -> Test
insertReplacesValue fuzzName implName fuzzer dictImpl =
    fuzz (Fuzz.pair fuzzer fuzzer)
        ("Inserts the same key with two different values into a " ++ implName ++ " and checks get returns the second value (" ++ fuzzName ++ ").")
        (\( key, val2 ) ->
            dictImpl.empty
                |> dictImpl.insert key key
                |> dictImpl.insert key val2
                |> dictImpl.get key
                |> Expect.equal (Just val2)
        )


removeNonExistentIsNoop : String -> String -> Fuzzer comparable -> IDict comparable comparable dict b dictb result x -> Test
removeNonExistentIsNoop fuzzName implName fuzzer dictImpl =
    fuzz fuzzer
        ("Removes a key from an empty " ++ implName ++ " and checks size is still 0 (" ++ fuzzName ++ ").")
        (\val ->
            dictImpl.empty
                |> dictImpl.remove val
                |> dictImpl.size
                |> Expect.equal 0
        )
