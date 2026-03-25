module TrieTest exposing (suite)

import DictIface exposing (IDict)
import Expect exposing (Expectation)
import Fuzz exposing (int, list, string)
import Fuzzers exposing (listChars, longString, prefixString, stringToListChars)
import Test exposing (Test, describe, test)
import Trie as Trie exposing (Trie)
import TrieIface exposing (ITrie)


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
    , isPrefix = Trie.isPrefix
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
        , DictIface.listOfValsAllKeysMembers "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , DictIface.listOfValsContainsAllVals "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsContainsAllVals "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , DictIface.listOfValsFoldlAllKeys "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsFoldlAllKeys "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , DictIface.listOfValsFoldlIncreasing "list (stringToListChars string)" " dict" (list (stringToListChars string)) trie
        , DictIface.listOfValsFoldlIncreasing "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , DictIface.listOfValsFoldrAllKeys "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsFoldrAllKeys "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , DictIface.listOfValsFoldrDecreasing "list (stringToListChars string)" " dict" (list (stringToListChars string)) trie
        , DictIface.listOfValsFoldrDecreasing "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , DictIface.listOfValsListsAllKeys "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsListsAllKeys "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , DictIface.listOfValsListsAllValues "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsListsAllValues "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , DictIface.listOfValsRemovedContainsNone "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsRemovedContainsNone "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , DictIface.listOfValsReportsSizeOk "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.listOfValsReportsSizeOk "list <| stringToListChars <| longString 10" "trie" (list <| stringToListChars <| longString 10) trie
        , DictIface.mapPreservesKeys "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.mapPreservesKeys "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , DictIface.filterTrueKeepsAll "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.filterFalseRemovesAll "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.partitionIsExhaustive "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.unionWithEmptyIsIdentity "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.unionContainsBothKeys "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.intersectOnlyCommonKeys "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.diffRemovesSecondKeys "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.fromListToListRoundtrip "list (stringToListChars string)" "trie" (list (stringToListChars string)) trie
        , DictIface.fromListToListRoundtrip "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , DictIface.insertReplacesValue "listChars" "trie" listChars trie
        , DictIface.removeNonExistentIsNoop "listChars" "trie" listChars trie
        , TrieIface.expandTest "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , TrieIface.expandResultsHavePrefix "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) listStartsWith trie
        , TrieIface.expandEmptyPrefixReturnsAll "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) [] trie
        , TrieIface.subtrieEmptyKeyReturnsSelf "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) [] trie
        , TrieIface.subtrieOfInsertedKeyHasValue "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) [] trie
        , TrieIface.isPrefixOfInsertedKey "list (stringToListChars prefixString)" "trie" (list (stringToListChars prefixString)) trie
        , emptyListKeyTest
        , matchBreakTest
        , matchContinueIfTest
        , matchContinueIfOneOfTest
        ]


listStartsWith : List comparable -> List comparable -> Bool
listStartsWith prefix full =
    List.take (List.length prefix) full == prefix


emptyListKeyTest : Test
emptyListKeyTest =
    test "singleton with empty list key" <|
        \_ ->
            Trie.singleton [] "val"
                |> Trie.get []
                |> Expect.equal (Just "val")


matchBreakTest : Test
matchBreakTest =
    let
        t =
            Trie.fromList
                [ ( [ 'a', 'b' ], "ab" )
                , ( [ 'a', 'c' ], "ac" )
                , ( [ 'b', 'x' ], "bx" )
                ]

        result =
            Trie.match
                (\_ _ ctx accum ->
                    ( accum + 1, ctx, Trie.break )
                )
                0
                ()
                t
    in
    test "match with Break only visits root node" <|
        \_ -> result |> Expect.equal 1


matchContinueIfTest : Test
matchContinueIfTest =
    let
        t =
            Trie.fromList
                [ ( [ 'a', 'b' ], "ab" )
                , ( [ 'a', 'c' ], "ac" )
                , ( [ 'b', 'x' ], "bx" )
                ]

        result =
            Trie.match
                (\maybeKeyPart maybeValue ctx accum ->
                    case maybeKeyPart of
                        Nothing ->
                            ( accum, ctx, Trie.continueIf 'a' )

                        Just _ ->
                            case maybeValue of
                                Nothing ->
                                    ( accum, ctx, Trie.wildcard )

                                Just val ->
                                    ( val :: accum, ctx, Trie.wildcard )
                )
                []
                ()
                t
    in
    test "match with ContinueIf only follows specified branch" <|
        \_ -> List.sort result |> Expect.equal [ "ab", "ac" ]


matchContinueIfOneOfTest : Test
matchContinueIfOneOfTest =
    let
        t =
            Trie.fromList
                [ ( [ 'a', 'b' ], "ab" )
                , ( [ 'a', 'c' ], "ac" )
                , ( [ 'b', 'x' ], "bx" )
                , ( [ 'c', 'y' ], "cy" )
                ]

        result =
            Trie.match
                (\maybeKeyPart maybeValue ctx accum ->
                    case maybeKeyPart of
                        Nothing ->
                            ( accum, ctx, Trie.continueIfOneOf [ 'a', 'b' ] )

                        Just _ ->
                            case maybeValue of
                                Nothing ->
                                    ( accum, ctx, Trie.wildcard )

                                Just val ->
                                    ( val :: accum, ctx, Trie.wildcard )
                )
                []
                ()
                t
    in
    test "match with ContinueIfOneOf follows specified branches" <|
        \_ -> List.sort result |> Expect.equal [ "ab", "ac", "bx" ]
