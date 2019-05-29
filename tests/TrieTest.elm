module TrieTest exposing (suite)

import DictIface exposing (IDict)
import Fuzz exposing (int, list, string)
import Fuzzers exposing (listChars, longString, stringToListChars, suffixString)
import Test exposing (Test, describe)
import Trie as Trie exposing (Trie)
import TrieIFace exposing (ITrie)


trie : ITrie (List comparable) a (Trie comparable a) b (Trie comparable b) result comparable context
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
    , match = Trie.match
    , expand = Trie.expand
    , isSuffix = Trie.isSuffix
    , subtrie = Trie.subtrie
    }


suite : Test
suite =
    describe "Trie tests"
        [ DictIface.emptyIsEmpty "trie" trie
        , DictIface.emptyContainsNoVal "listChars" "trie" listChars trie
        , DictIface.emptyInsertStringContainsVal "listChars" "trie" listChars trie
        , DictIface.nonEmptyIsNotEmpty "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.singletonContainsVal "listChars" "trie" listChars trie
        , DictIface.listOfValsAllKeysMembers "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsAllKeysMembers "list (stringToListChars suffixString)" "trie" (list (stringToListChars suffixString)) trie
        , DictIface.listOfValsContainsAllVals "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsContainsAllVals "list (stringToListChars suffixString)" "trie" (list (stringToListChars suffixString)) trie
        , DictIface.listOfValsFoldlAllKeys "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsFoldlAllKeys "list (stringToListChars suffixString)" "trie" (list (stringToListChars suffixString)) trie
        , DictIface.listOfValsFoldlIncreasing "list (stringToListChars string)" " dict" (list (stringToListChars string)) trie
        , DictIface.listOfValsFoldlIncreasing "list (stringToListChars suffixString)" "trie" (list (stringToListChars suffixString)) trie
        , DictIface.listOfValsFoldrAllKeys "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsFoldrAllKeys "list (stringToListChars suffixString)" "trie" (list (stringToListChars suffixString)) trie
        , DictIface.listOfValsFoldrDecreasing "list (stringToListChars string)" " dict" (list (stringToListChars string)) trie
        , DictIface.listOfValsFoldrDecreasing "list (stringToListChars suffixString)" "trie" (list (stringToListChars suffixString)) trie
        , DictIface.listOfValsListsAllKeys "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsListsAllKeys "list (stringToListChars suffixString)" "trie" (list (stringToListChars suffixString)) trie
        , DictIface.listOfValsListsAllValues "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsListsAllValues "list (stringToListChars suffixString)" "trie" (list (stringToListChars suffixString)) trie
        , DictIface.listOfValsRemovedContainsNone "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsRemovedContainsNone "list (stringToListChars suffixString)" "trie" (list (stringToListChars suffixString)) trie
        , DictIface.listOfValsReportsSizeOk "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsReportsSizeOk "list <| stringToListChars <| longString 10" "trie" (list <| stringToListChars <| longString 10) trie
        ]
