module Example exposing (emptyContainsNoVal, singletonContainsVal)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Trie


emptyContainsNoVal : Test
emptyContainsNoVal =
    describe "emptyContainsNoVal"
        [ fuzz string "creates singleton tries" <|
            \val ->
                Trie.empty |> Trie.get val |> Expect.equal Nothing
        ]


singletonContainsVal : Test
singletonContainsVal =
    describe "singletonContainsVal"
        [ fuzz string "creates singleton tries" <|
            \val ->
                Trie.singleton val val |> Trie.get val |> Expect.equal (Just val)
        ]
