module TrieTest exposing (suite)

import DictIface exposing (IDict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Fuzzers exposing (..)
import Set
import Test exposing (..)
import Trie exposing (Trie)


trieDict : IDict String a (Trie a)
trieDict =
    { empty = Trie.empty
    , singleton = Trie.singleton
    , get = Trie.get
    , insert = Trie.insert
    }


suite : Test
suite =
    describe "Trie tests"
        [ DictIface.listOfValsContainsAllVals "list string" "trieDict" (list string) trieDict
        , DictIface.listOfValsContainsAllVals "list suffixString" "trieDict" (list suffixString) trieDict
        ]
