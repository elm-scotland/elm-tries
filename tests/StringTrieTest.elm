module StringTrieTest exposing (suite)

import DictIface exposing (IDict)
import Fuzz exposing (int, list, string)
import Fuzzers exposing (longString, suffixString)
import StringTrie as Trie exposing (Trie)
import Test exposing (Test, describe)


trie : IDict String a (Trie a) b (Trie b) result
trie =
    { empty = Trie.empty
    , singleton = Trie.singleton
    , insert = Trie.insert
    , update = Trie.update
    , remove = Trie.remove
    , isEmpty = Trie.isEmpty
    , member = Trie.member
    , get = Trie.get
    , size = Trie.size
    , keys = Trie.keys
    , values = Trie.values
    , toList = Trie.toList
    , fromList = Trie.fromList
    , map = Trie.map
    , foldl = Trie.foldl
    , foldr = Trie.foldr
    , filter = Trie.filter
    , partition = Trie.partition
    , union = Trie.union
    , intersect = Trie.intersect
    , diff = Trie.diff
    , merge = Trie.merge
    }


suite : Test
suite =
    describe "Trie tests"
        [ DictIface.emptyIsEmpty "trie" trie
        , DictIface.emptyContainsNoVal "string" "trie" string trie
        , DictIface.emptyInsertStringContainsVal "string" "trie" string trie
        , DictIface.nonEmptyIsNotEmpty "list string" "trie" (list string) trie
        , DictIface.singletonContainsVal "string" "trie" string trie
        , DictIface.singletonEmptyStringContainsVal "string" "trie" string trie
        , DictIface.listOfNumsDoubledAllEven "list int" "trie" (list int) trie
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
