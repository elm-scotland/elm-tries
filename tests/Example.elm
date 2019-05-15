module Example exposing
    ( emptyContainsNoVal
    , emptyInsertStringContainsVal
    , listOfNumsDoubledAllEven
    , listOfValsContainsAllVals
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



-- Fuzzers for better trie testing.


wordList =
    Fuzz.frequency
        [ ( 4, Fuzz.constant "a" )
        , ( 3, Fuzz.constant "b" )
        , ( 2, Fuzz.constant "c" )
        , ( 1, Fuzz.constant "d" )
        , ( 1, Fuzz.constant "" )
        ]
        |> Fuzz.list
        |> Fuzz.map (String.concat >> String.left 10)
        |> Fuzz.list


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
        , fuzz wordList
            "Creates a trie with a list of vals and ensures it contains all of them (wordList)."
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
            "Creates a trie with a list of vals and checks it has the correct size."
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
        , fuzz wordList
            "Creates a trie with a list of vals, removes all, and checks none are present (wordList)."
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
        , fuzz wordList
            "Creates a trie with a list of vals and ensures lists all of them as keys (wordList)."
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
        , fuzz wordList
            "Creates a trie with a list of vals and ensures lists all of them as values (wordList)."
            test
        ]
