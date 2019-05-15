module Example exposing
    ( emptyContainsNoVal
    , emptyInsertStringContainsVal
    , listOfNumsDoubledAllEven
    , listOfValsAllKeysMembers
    , listOfValsContainsAllVals
    , listOfValsFoldlAllKeys
    , listOfValsFoldrAllKeys
    , listOfValsListsAllKeys
    , listOfValsListsAllValues
    , listOfValsRemovedContainsNone
    , listOfValsReportsSizeOk
    , singletonContainsVal
    , singletonEmptyStringContainsVal
    )

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Set
import Test exposing (..)
import Trie



-- Fuzzers targeted at trie testing.


suffixString =
    Fuzz.frequency
        [ ( 4, Fuzz.constant "a" )
        , ( 3, Fuzz.constant "b" )
        , ( 2, Fuzz.constant "c" )
        , ( 1, Fuzz.constant "d" )
        , ( 1, Fuzz.constant "" )
        ]
        |> Fuzz.list
        |> Fuzz.map (String.concat >> String.left 10)


longString factor =
    Fuzz.frequency
        [ ( 1, Fuzz.constant (String.repeat factor "a") )
        , ( 1, Fuzz.constant (String.repeat factor "b") )
        , ( 1, Fuzz.constant (String.repeat factor "c") )
        , ( 1, Fuzz.constant (String.repeat factor "d") )
        ]
        |> Fuzz.list
        |> Fuzz.map String.concat


emptyContainsNoVal : Test
emptyContainsNoVal =
    describe "emptyContainsNoVal"
        [ fuzz string "Creates singleton tries." <|
            \val ->
                Trie.empty |> Trie.get val |> Expect.equal Nothing
        ]


singletonContainsVal : Test
singletonContainsVal =
    describe "singletonContainsVal"
        [ fuzz string "Creates singleton tries." <|
            \val ->
                Trie.singleton val val |> Trie.get val |> Expect.equal (Just val)
        ]


singletonEmptyStringContainsVal : Test
singletonEmptyStringContainsVal =
    describe "singletonEmptyStringContainsVal"
        [ fuzz string "Creates singleton tries with the empty string as key." <|
            \val ->
                Trie.singleton "" val |> Trie.get "" |> Expect.equal (Just val)
        ]


emptyInsertStringContainsVal : Test
emptyInsertStringContainsVal =
    describe "emptyInsertStringContainsVal"
        [ fuzz string "Creates tries by inserting to empty." <|
            \val ->
                Trie.empty |> Trie.insert val val |> Trie.get val |> Expect.equal (Just val)
        ]


listOfValsContainsAllVals : Test
listOfValsContainsAllVals =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> Trie.insert val val trie) Trie.empty vals
                        |> Expect.all (List.map (\val trie -> Trie.get val trie |> Expect.equal (Just val)) vals)
    in
    describe "listOfValsContainsAllVals"
        [ fuzz (list string)
            "Creates a trie with a list of vals and ensures it contains all of them (list string)."
            test
        , fuzz (list suffixString)
            "Creates a trie with a list of vals and ensures it contains all of them (list suffixString)."
            test
        ]


listOfValsReportsSizeOk : Test
listOfValsReportsSizeOk =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal 0 (Trie.size Trie.empty)

                vals ->
                    List.foldl (\val trie -> Trie.insert val val trie) Trie.empty vals
                        |> Trie.size
                        |> Expect.equal (Set.size (Set.fromList vals))
    in
    describe "listOfValsReportsSizeOk"
        [ fuzz (list string)
            "Creates a trie with a list of vals and checks it has the correct size (list string)."
            test
        , fuzz (list <| longString 10)
            "Creates a trie with a list of vals and checks it has the correct size (list <| longString 10)."
            test
        ]


listOfNumsDoubledAllEven : Test
listOfNumsDoubledAllEven =
    let
        doubleOdd keys trie =
            List.foldl
                (\key accum ->
                    Trie.update (String.fromInt key)
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
                    List.foldl (\val trie -> Trie.insert (String.fromInt val) val trie) Trie.empty vals
                        |> doubleOdd vals
                        |> Expect.all (List.map (\val trie -> Trie.get (String.fromInt val) trie |> expectJustEven) vals)
    in
    describe "listOfNumsDoubledAllEven"
        [ fuzz (list int)
            "Creates a trie with a list of numbers, then updates to double all numbers that are odd, and checks all vals are even."
            test
        ]


listOfValsAllKeysMembers : Test
listOfValsAllKeysMembers =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> Trie.insert val val trie) Trie.empty vals
                        |> (\trie -> Expect.all (List.map (\val list -> Trie.member val trie |> Expect.true "not member of trie") vals) trie)
    in
    describe "listOfValsAllKeysMembers"
        [ fuzz (list string)
            "Creates a trie with a list of vals and ensures all keys are members (list string)."
            test
        , fuzz (list suffixString)
            "Creates a trie with a list of vals and ensures all keys are members (list suffixString)."
            test
        ]


listOfValsRemovedContainsNone : Test
listOfValsRemovedContainsNone =
    let
        removeAll keys trie =
            List.foldl (\key accum -> Trie.remove key accum) trie keys

        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> Trie.insert val val trie) Trie.empty vals
                        |> removeAll vals
                        |> Expect.all (List.map (\val trie -> Trie.get val trie |> Expect.equal Nothing) vals)
    in
    describe "listOfValsRemovedContainsNone"
        [ fuzz (list string)
            "Creates a trie with a list of vals, removes all, and checks none are present (list string)."
            test
        , fuzz (list suffixString)
            "Creates a trie with a list of vals, removes all, and checks none are present (list suffixString)."
            test
        ]


listOfValsListsAllKeys : Test
listOfValsListsAllKeys =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> Trie.insert val val trie) Trie.empty vals
                        |> Trie.keys
                        |> Expect.all (List.map (\val list -> List.member val list |> Expect.true "not member of keys") vals)
    in
    describe "listOfValsListsAllKeys"
        [ fuzz (list string)
            "Creates a trie with a list of vals and ensures lists all of them as keys (list string)."
            test
        , fuzz (list suffixString)
            "Creates a trie with a list of vals and ensures lists all of them as keys (list suffixString)."
            test
        ]


listOfValsListsAllValues : Test
listOfValsListsAllValues =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> Trie.insert val val trie) Trie.empty vals
                        |> Trie.values
                        |> Expect.all (List.map (\val list -> List.member val list |> Expect.true "not member of keys") vals)
    in
    describe "listOfValsListsAllValues"
        [ fuzz (list string)
            "Creates a trie with a list of vals and ensures lists all of them as values (list string)."
            test
        , fuzz (list suffixString)
            "Creates a trie with a list of vals and ensures lists all of them as values (list suffixString)."
            test
        ]


listOfValsFoldlAllKeys : Test
listOfValsFoldlAllKeys =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> Trie.insert val val trie) Trie.empty vals
                        |> Trie.foldl (\k _ accum -> Set.insert k accum) Set.empty
                        |> Expect.all (List.map (\val list -> Set.member val list |> Expect.true "not member of foldl keys") vals)
    in
    describe "listOfValsFoldlAllKeys"
        [ fuzz (list string)
            "Creates a trie with a list of vals and folds it left, checking all keys are gathered in the fold (list string)."
            test
        , fuzz (list suffixString)
            "Creates a trie with a list of vals and folds it left, checking all keys are gathered in the fold (list suffixString)."
            test
        ]


listOfValsFoldrAllKeys : Test
listOfValsFoldrAllKeys =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> Trie.insert val val trie) Trie.empty vals
                        |> Trie.foldr (\k _ accum -> Set.insert k accum) Set.empty
                        |> Expect.all (List.map (\val list -> Set.member val list |> Expect.true "not member of foldl keys") vals)
    in
    describe "listOfValsFoldrAllKeys"
        [ fuzz (list string)
            "Creates a trie with a list of vals and folds it right, checking all keys are gathered in the fold (list string)."
            test
        , fuzz (list suffixString)
            "Creates a trie with a list of vals and folds it right, checking all keys are gathered in the fold (list suffixString)."
            test
        ]
