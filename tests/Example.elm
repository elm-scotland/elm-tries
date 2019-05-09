module Example exposing (emptyContainsNoVal, listOfValsContainsAllVals, singletonContainsVal, singletonEmptyStringContainsVal)

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


listOfValsContainsAllVals : Test
listOfValsContainsAllVals =
    describe "listOfValsContainsAllVals"
        [ fuzz (list string) "Creates a trie with a list of vals and ensures it contains all of them." <|
            \possiblyEmptyVals ->
                case possiblyEmptyVals of
                    [] ->
                        Expect.equal [] []

                    vals ->
                        List.foldl (\val trie -> trie) Trie.empty vals
                            |> Expect.all (List.map (\val trie -> Trie.get val trie |> Expect.equal (Just val)) vals)
        ]
