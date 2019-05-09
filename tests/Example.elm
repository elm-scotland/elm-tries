module Example exposing (singleton)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Trie


singleton : Test
singleton =
    describe "singleton"
        [ fuzz string "creates singleton tries" <|
            \val ->
                Trie.singleton val () |> Trie.get val |> Expect.equal (Just ())
        ]
