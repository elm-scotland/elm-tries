module StringTrieTest exposing (suite)

import DictIface exposing (IDict)
import Expect exposing (Expectation)
import Fuzz exposing (int, list, string)
import Fuzzers exposing (longString, prefixString)
import StringTrie as Trie exposing (Trie)
import Test exposing (Test, describe, test)
import TrieIface exposing (ITrie)


trie : ITrie String a (Trie a) b (Trie b) result Char context
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
    , isPrefix = Trie.isPrefix
    , subtrie = Trie.subtrie
    }


suite : Test
suite =
    describe "StringTrie tests"
        [ DictIface.emptyIsEmpty "trie" trie
        , DictIface.emptyContainsNoVal "string" "trie" string trie
        , DictIface.emptyInsertStringContainsVal "string" "trie" string trie
        , DictIface.nonEmptyIsNotEmpty "list string" "trie" (list string) trie
        , DictIface.singletonContainsVal "string" "trie" string trie
        , DictIface.singletonEmptyStringContainsVal "string" "trie" string trie
        , DictIface.listOfNumsDoubledAllEven "list int" "trie" (list int) trie
        , DictIface.listOfValsAllKeysMembers "list string" "trie" (list string) trie
        , DictIface.listOfValsAllKeysMembers "list prefixString" "trie" (list prefixString) trie
        , DictIface.listOfValsContainsAllVals "list string" "trie" (list string) trie
        , DictIface.listOfValsContainsAllVals "list prefixString" "trie" (list prefixString) trie
        , DictIface.listOfValsFoldlAllKeys "list string" "trie" (list string) trie
        , DictIface.listOfValsFoldlAllKeys "list prefixString" "trie" (list prefixString) trie
        , DictIface.listOfValsFoldlIncreasing "list string" " dict" (list string) trie
        , DictIface.listOfValsFoldlIncreasing "list prefixString" "trie" (list prefixString) trie
        , DictIface.listOfValsFoldrAllKeys "list string" "trie" (list string) trie
        , DictIface.listOfValsFoldrAllKeys "list prefixString" "trie" (list prefixString) trie
        , DictIface.listOfValsFoldrDecreasing "list string" " dict" (list string) trie
        , DictIface.listOfValsFoldrDecreasing "list prefixString" "trie" (list prefixString) trie
        , DictIface.listOfValsListsAllKeys "list string" "trie" (list string) trie
        , DictIface.listOfValsListsAllKeys "list prefixString" "trie" (list prefixString) trie
        , DictIface.listOfValsListsAllValues "list string" "trie" (list string) trie
        , DictIface.listOfValsListsAllValues "list prefixString" "trie" (list prefixString) trie
        , DictIface.listOfValsRemovedContainsNone "list string" "trie" (list string) trie
        , DictIface.listOfValsRemovedContainsNone "list prefixString" "trie" (list prefixString) trie
        , DictIface.listOfValsReportsSizeOk "list string" "trie" (list string) trie
        , DictIface.listOfValsReportsSizeOk "list <| longString 10" "trie" (list <| longString 10) trie
        , TrieIface.expandTest "list prefixString" "trie" (list prefixString) trie
        , expandIgnoreCaseTest
        ]


expandIgnoreCaseTest : Test
expandIgnoreCaseTest =
    let
        empty =
            Trie.empty

        example =
            Trie.fromList
                [ ( "abc", True )
                , ( "aD", True )
                , ( "AbD", True )
                , ( "Ab", True )
                , ( "ABC", True )
                ]
    in
    describe "A StringTrie.Trie"
        [ describe "when empty"
            [ test "expands ignoring case to and empty list on empty key" <|
                \_ ->
                    Trie.expandIgnoreCase "" empty
                        |> Expect.equal []
            , test "expands ignoring case to and empty list on non-empty key" <|
                \_ ->
                    Trie.expandIgnoreCase "a" empty
                        |> Expect.equal []
            ]
        , describe "with keys 'aD' 'abc' 'Abc' 'ABC'"
            [ test "expands to all keys on empty key" <|
                \_ ->
                    Trie.expandIgnoreCase "" example
                        |> Expect.equal
                            [ ( "abc", True )
                            , ( "aD", True )
                            , ( "AbD", True )
                            , ( "Ab", True )
                            , ( "ABC", True )
                            ]
            , test "expands to all AB keys on 'ab' key" <|
                \_ ->
                    Trie.expandIgnoreCase "ab" example
                        |> Expect.equal
                            [ ( "abc", True )
                            , ( "AbD", True )
                            , ( "Ab", True )
                            , ( "ABC", True )
                            ]
            , test "expands to all AB keys on 'AB' key" <|
                \_ ->
                    Trie.expandIgnoreCase "AB" example
                        |> Expect.equal
                            [ ( "abc", True )
                            , ( "AbD", True )
                            , ( "Ab", True )
                            , ( "ABC", True )
                            ]
            , test "expands to empty list on key that is too long 'abcX'" <|
                \_ ->
                    Trie.expandIgnoreCase "abcX" example
                        |> Expect.equal []
            ]
        ]
