module Example exposing
    ( emptyContainsNoVal
    , emptyInsertStringContainsVal
    , listOfNumsDoubledAllEven
    , listOfValsContainsAllVals
    , listOfValsReportsSizeOk
    , singletonContainsVal
    , singletonEmptyStringContainsVal
    )

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Set
import Test exposing (..)
import Trie


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
    describe "listOfValsContainsAllVals"
        [ fuzz (list string) "Creates a trie with a list of vals and ensures it contains all of them." <|
            \possiblyEmptyVals ->
                case possiblyEmptyVals of
                    [] ->
                        Expect.equal [] []

                    vals ->
                        List.foldl (\val trie -> Trie.insert val val trie) Trie.empty vals
                            |> Expect.all (List.map (\val trie -> Trie.get val trie |> Expect.equal (Just val)) vals)
        ]


listOfValsReportsSizeOk : Test
listOfValsReportsSizeOk =
    describe "listOfValsReportsSizeOk"
        [ fuzz (list string) "Creates a trie with a list of vals and checks it has the correct size." <|
            \possiblyEmptyVals ->
                case possiblyEmptyVals of
                    [] ->
                        Expect.equal 0 (Trie.size Trie.empty)

                    vals ->
                        List.foldl (\val trie -> Trie.insert val val trie) Trie.empty vals
                            |> Trie.size
                            |> Expect.equal (Set.size (Set.fromList vals))
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
    in
    describe "listOfNumsDoubledAllEven"
        [ fuzz (list int) "Creates a trie with a list of numbers, then updates to double all numbers that are odd, and checks all vals are even." <|
            \possiblyEmptyVals ->
                case possiblyEmptyVals of
                    [] ->
                        Expect.equal [] []

                    vals ->
                        List.foldl (\val trie -> Trie.insert (String.fromInt val) val trie) Trie.empty vals
                            |> doubleOdd vals
                            |> Expect.all (List.map (\val trie -> Trie.get (String.fromInt val) trie |> expectJustEven) vals)
        ]
