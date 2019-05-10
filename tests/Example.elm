module Example exposing
    ( emptyContainsNoVal
    , emptyInsertStringContainsVal
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
