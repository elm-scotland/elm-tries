module TrieTest exposing (suite)

import DictIface exposing (IDict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Fuzzers exposing (..)
import Set
import Test exposing (..)
import Trie exposing (Trie)


trie : IDict String a (Trie a)
trie =
    { empty = Trie.empty
    , singleton = Trie.singleton
    , get = Trie.get
    , insert = Trie.insert
    }


suite : Test
suite =
    describe "Trie tests"
        [ DictIface.listOfNumsDoubledAllEven "list int" "trie" (list int) trie
        , DictIface.listOfValsAllKeysMembers "list string" "trie" (list string) trie
        , DictIface.listOfValsAllKeysMembers "list suffixString" "trie" (list suffixString) trie
        , DictIface.listOfValsContainsAllVals "list string" "trie" (list string) trie
        , DictIface.listOfValsContainsAllVals "list suffixString" "trie" (list suffixString) trie
        , DictIface.listOfValsFoldlAllKeys "list string" "trie" (list string) trie
        , DictIface.listOfValsFoldlAllKeys "list suffixString" "trie" (list suffixString) trie
        , DictIface.listOfValsFoldrAllKeys "list string" "trie" (list string) trie
        , DictIface.listOfValsFoldrAllKeys "list suffixString" "trie" (list suffixString) trie
        , DictIface.listOfValsListsAllKeys "list string" "trie" (list string) trie
        , DictIface.listOfValsListsAllKeys "list suffixString" "trie" (list suffixString) trie
        , DictIface.listOfValsListsAllValues "list string" "trie" (list string) trie
        , DictIface.listOfValsListsAllValues "list suffixString" "trie" (list suffixString) trie
        , DictIface.listOfValsRemovedContainsNone "list string" "trie" (list string) trie
        , DictIface.listOfValsRemovedContainsNone "list suffixString" "trie" (list suffixString) trie
        , DictIface.listOfValsReportsSizeOk "list string" "trie" (list string) trie
        , DictIface.listOfValsReportsSizeOk "list <| longString 10" "trie" (list <| longString 10) trie
        ]
