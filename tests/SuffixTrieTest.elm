module SuffixTrieTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (int, list, string)
import Fuzzers exposing (longString, prefixString)
import Set
import SuffixTrie exposing (SuffixTrie)
import Test exposing (Test, describe, fuzz, test)


{-| Helper to build a SuffixTrie from a list of values used as both key and value.
-}
buildTrie : List String -> SuffixTrie String
buildTrie vals =
    List.foldl (\v st -> SuffixTrie.insert v v st) SuffixTrie.empty vals


suite : Test
suite =
    describe "SuffixTrie tests"
        [ dictTests
        , substringTests
        , suffixTests
        , prefixTests
        , edgeCaseTests
        ]


dictTests : Test
dictTests =
    describe "Dict-like operations"
        [ test "empty is empty" <|
            \_ ->
                SuffixTrie.isEmpty SuffixTrie.empty |> Expect.equal True
        , fuzz prefixString "empty contains no val" <|
            \val ->
                SuffixTrie.get val SuffixTrie.empty |> Expect.equal Nothing
        , fuzz prefixString "insert into empty then get" <|
            \val ->
                SuffixTrie.empty
                    |> SuffixTrie.insert val val
                    |> SuffixTrie.get val
                    |> Expect.equal (Just val)
        , fuzz (list prefixString) "non-empty is not empty" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        buildTrie vals |> SuffixTrie.isEmpty |> Expect.equal False
        , fuzz prefixString "singleton contains val" <|
            \val ->
                SuffixTrie.singleton val val |> SuffixTrie.get val |> Expect.equal (Just val)
        , fuzz prefixString "singleton with empty string key" <|
            \val ->
                SuffixTrie.singleton "" val |> SuffixTrie.get "" |> Expect.equal (Just val)
        , fuzz (list prefixString) "all inserted vals are retrievable" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            st =
                                buildTrie vals
                        in
                        List.all (\v -> SuffixTrie.get v st == Just v) vals
                            |> Expect.equal True
        , fuzz (list prefixString) "all keys are members" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            st =
                                buildTrie vals
                        in
                        List.all (\v -> SuffixTrie.member v st) vals
                            |> Expect.equal True
        , fuzz (list prefixString) "size matches unique keys" <|
            \vals ->
                case vals of
                    [] ->
                        SuffixTrie.size SuffixTrie.empty |> Expect.equal 0

                    _ ->
                        buildTrie vals
                            |> SuffixTrie.size
                            |> Expect.equal (Set.size (Set.fromList vals))
        , fuzz (list prefixString) "keys lists all inserted keys" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            ks =
                                SuffixTrie.keys (buildTrie vals)
                        in
                        List.all (\v -> List.member v ks) vals
                            |> Expect.equal True
        , fuzz (list prefixString) "values lists all inserted values" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            vs =
                                SuffixTrie.values (buildTrie vals)
                        in
                        List.all (\v -> List.member v vs) vals
                            |> Expect.equal True
        , fuzz (list prefixString) "foldl gathers all keys" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            foldedKeys =
                                SuffixTrie.foldl (\k _ acc -> Set.insert k acc) Set.empty (buildTrie vals)
                        in
                        List.all (\v -> Set.member v foldedKeys) vals
                            |> Expect.equal True
        , fuzz (list prefixString) "foldl visits keys in increasing order" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            foldedKeys =
                                SuffixTrie.foldl (\k _ acc -> k :: acc) [] (buildTrie vals)
                                    |> List.reverse
                        in
                        foldedKeys
                            |> Expect.equal (List.sort foldedKeys)
        , fuzz (list prefixString) "foldr gathers all keys" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            foldedKeys =
                                SuffixTrie.foldr (\k _ acc -> Set.insert k acc) Set.empty (buildTrie vals)
                        in
                        List.all (\v -> Set.member v foldedKeys) vals
                            |> Expect.equal True
        , fuzz (list prefixString) "foldr visits keys in decreasing order" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            foldedKeys =
                                SuffixTrie.foldr (\k _ acc -> k :: acc) [] (buildTrie vals)
                        in
                        foldedKeys
                            |> Expect.equal (List.sort foldedKeys)
        , fuzz (list prefixString) "remove all then empty" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            st =
                                buildTrie vals

                            removed =
                                List.foldl SuffixTrie.remove st vals
                        in
                        List.all (\v -> SuffixTrie.get v removed == Nothing) vals
                            |> Expect.equal True
        , fuzz (list prefixString) "map preserves keys" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            mapped =
                                SuffixTrie.map (\_ v -> v) (buildTrie vals)

                            mappedKeys =
                                SuffixTrie.keys mapped
                        in
                        List.all (\v -> List.member v mappedKeys) vals
                            |> Expect.equal True
        , fuzz (list prefixString) "filter true keeps all" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            st =
                                buildTrie vals
                        in
                        SuffixTrie.filter (\_ _ -> True) st
                            |> SuffixTrie.size
                            |> Expect.equal (SuffixTrie.size st)
        , fuzz (list prefixString) "filter false removes all" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        SuffixTrie.filter (\_ _ -> False) (buildTrie vals)
                            |> SuffixTrie.isEmpty
                            |> Expect.equal True
        , fuzz (list prefixString) "partition is exhaustive" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            st =
                                buildTrie vals

                            uniqueKeys =
                                SuffixTrie.keys st

                            halfKeys =
                                Set.fromList (List.take (List.length uniqueKeys // 2) uniqueKeys)

                            ( t1, t2 ) =
                                SuffixTrie.partition (\k _ -> Set.member k halfKeys) st
                        in
                        (SuffixTrie.size t1 + SuffixTrie.size t2)
                            |> Expect.equal (SuffixTrie.size st)
        , fuzz (list prefixString) "union with empty is identity" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            st =
                                buildTrie vals
                        in
                        SuffixTrie.union st SuffixTrie.empty
                            |> SuffixTrie.size
                            |> Expect.equal (SuffixTrie.size st)
        , fuzz (list prefixString) "union contains both keys" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            half =
                                List.length vals // 2

                            t1 =
                                buildTrie (List.take half vals)

                            t2 =
                                buildTrie (List.drop half vals)

                            combined =
                                SuffixTrie.union t1 t2
                        in
                        List.all (\v -> SuffixTrie.member v combined) vals
                            |> Expect.equal True
        , fuzz (list prefixString) "intersect only common keys" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            t1 =
                                buildTrie vals

                            subsetVals =
                                List.take (List.length vals // 2) vals

                            t2 =
                                buildTrie subsetVals

                            inter =
                                SuffixTrie.intersect t1 t2

                            interKeys =
                                SuffixTrie.keys inter
                        in
                        List.all (\k -> SuffixTrie.member k t1 && SuffixTrie.member k t2) interKeys
                            |> Expect.equal True
        , fuzz (list prefixString) "diff removes second keys" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            t1 =
                                buildTrie vals

                            subsetVals =
                                List.take (List.length vals // 2) vals

                            t2 =
                                buildTrie subsetVals

                            d =
                                SuffixTrie.diff t1 t2
                        in
                        List.all (\v -> not (SuffixTrie.member v d)) subsetVals
                            |> Expect.equal True
        , fuzz (list prefixString) "fromList toList roundtrip" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            pairs =
                                List.map (\v -> ( v, v )) vals

                            resultKeys =
                                SuffixTrie.fromList pairs |> SuffixTrie.toList |> List.map Tuple.first |> Set.fromList
                        in
                        resultKeys |> Expect.equal (Set.fromList vals)
        , fuzz (Fuzz.pair prefixString prefixString) "insert replaces value" <|
            \( key, val2 ) ->
                SuffixTrie.empty
                    |> SuffixTrie.insert key key
                    |> SuffixTrie.insert key val2
                    |> SuffixTrie.get key
                    |> Expect.equal (Just val2)
        , fuzz prefixString "remove non-existent is noop" <|
            \val ->
                SuffixTrie.empty
                    |> SuffixTrie.remove val
                    |> SuffixTrie.size
                    |> Expect.equal 0
        , fuzz (list (Fuzz.map String.fromInt int)) "update doubles all values, all even" <|
            \vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            st =
                                List.foldl (\v acc -> SuffixTrie.insert v (Maybe.withDefault 0 (String.toInt v)) acc) SuffixTrie.empty vals

                            doubled =
                                List.foldl
                                    (\v acc ->
                                        SuffixTrie.update v
                                            (Maybe.map (\n -> n * 2))
                                            acc
                                    )
                                    st
                                    vals
                        in
                        List.all
                            (\v ->
                                case SuffixTrie.get v doubled of
                                    Just n ->
                                        modBy 2 n == 0

                                    Nothing ->
                                        False
                            )
                            vals
                            |> Expect.equal True
        ]


substringTests : Test
substringTests =
    let
        trie =
            SuffixTrie.fromList
                [ ( "banana", 1 )
                , ( "canal", 2 )
                , ( "bandana", 3 )
                ]
    in
    describe "Substring search"
        [ test "isSubstring finds 'an' in banana and canal" <|
            \_ ->
                SuffixTrie.isSubstring "an" trie
                    |> Expect.equal True
        , test "isSubstring returns False for missing substring" <|
            \_ ->
                SuffixTrie.isSubstring "xyz" trie
                    |> Expect.equal False
        , test "isSubstring with empty string is True for non-empty trie" <|
            \_ ->
                SuffixTrie.isSubstring "" trie
                    |> Expect.equal True
        , test "isSubstring with empty string on empty trie is False" <|
            \_ ->
                SuffixTrie.isSubstring "" SuffixTrie.empty
                    |> Expect.equal False
        , test "findSubstring 'an' returns banana, canal, bandana" <|
            \_ ->
                SuffixTrie.findSubstring "an" trie
                    |> List.map Tuple.first
                    |> List.sort
                    |> Expect.equal [ "banana", "bandana", "canal" ]
        , test "findSubstring 'ban' returns banana and bandana" <|
            \_ ->
                SuffixTrie.findSubstring "ban" trie
                    |> List.map Tuple.first
                    |> List.sort
                    |> Expect.equal [ "banana", "bandana" ]
        , test "findSubstring returns correct values" <|
            \_ ->
                SuffixTrie.findSubstring "canal" trie
                    |> Expect.equal [ ( "canal", 2 ) ]
        , test "findSubstring 'xyz' returns empty list" <|
            \_ ->
                SuffixTrie.findSubstring "xyz" trie
                    |> Expect.equal []
        , test "findSubstringWithPositions 'an' in banana" <|
            \_ ->
                SuffixTrie.fromList [ ( "banana", 1 ) ]
                    |> SuffixTrie.findSubstringWithPositions "an"
                    |> Expect.equal [ ( "banana", 1, [ 1, 3 ] ) ]
        , test "findSubstringWithPositions 'na' in banana" <|
            \_ ->
                SuffixTrie.fromList [ ( "banana", 1 ) ]
                    |> SuffixTrie.findSubstringWithPositions "na"
                    |> Expect.equal [ ( "banana", 1, [ 2, 4 ] ) ]
        , test "findSubstringWithPositions with full key" <|
            \_ ->
                SuffixTrie.fromList [ ( "banana", 1 ) ]
                    |> SuffixTrie.findSubstringWithPositions "banana"
                    |> Expect.equal [ ( "banana", 1, [ 0 ] ) ]
        , test "findSubstringIgnoreCase 'AN' finds same as 'an'" <|
            \_ ->
                SuffixTrie.findSubstringIgnoreCase "AN" trie
                    |> List.map Tuple.first
                    |> List.sort
                    |> Expect.equal [ "banana", "bandana", "canal" ]
        , test "findSubstringIgnoreCase with mixed case keys" <|
            \_ ->
                SuffixTrie.fromList [ ( "Hello", 1 ), ( "HELLO", 2 ), ( "world", 3 ) ]
                    |> SuffixTrie.findSubstringIgnoreCase "ell"
                    |> List.map Tuple.first
                    |> List.sort
                    |> Expect.equal [ "HELLO", "Hello" ]
        ]


suffixTests : Test
suffixTests =
    let
        trie =
            SuffixTrie.fromList
                [ ( "banana", 1 )
                , ( "canal", 2 )
                , ( "bandana", 3 )
                ]
    in
    describe "Suffix search"
        [ test "isSuffix 'na' is True (banana ends with 'na')" <|
            \_ ->
                SuffixTrie.isSuffix "na" trie
                    |> Expect.equal True
        , test "isSuffix 'ana' is True (banana and bandana end with 'ana')" <|
            \_ ->
                SuffixTrie.isSuffix "ana" trie
                    |> Expect.equal True
        , test "isSuffix 'al' is True (canal ends with 'al')" <|
            \_ ->
                SuffixTrie.isSuffix "al" trie
                    |> Expect.equal True
        , test "isSuffix 'an' is False (no key ends with 'an')" <|
            \_ ->
                SuffixTrie.isSuffix "an" trie
                    |> Expect.equal False
        , test "isSuffix 'xyz' is False" <|
            \_ ->
                SuffixTrie.isSuffix "xyz" trie
                    |> Expect.equal False
        , test "findSuffix 'ana' returns banana and bandana" <|
            \_ ->
                SuffixTrie.findSuffix "ana" trie
                    |> List.map Tuple.first
                    |> List.sort
                    |> Expect.equal [ "banana", "bandana" ]
        , test "findSuffix 'al' returns canal" <|
            \_ ->
                SuffixTrie.findSuffix "al" trie
                    |> Expect.equal [ ( "canal", 2 ) ]
        , test "findSuffix 'xyz' returns empty list" <|
            \_ ->
                SuffixTrie.findSuffix "xyz" trie
                    |> Expect.equal []
        , test "findSuffix with full key" <|
            \_ ->
                SuffixTrie.findSuffix "banana" trie
                    |> Expect.equal [ ( "banana", 1 ) ]
        ]


prefixTests : Test
prefixTests =
    let
        trie =
            SuffixTrie.fromList
                [ ( "abc", 1 )
                , ( "abd", 2 )
                , ( "xyz", 3 )
                ]
    in
    describe "Prefix search"
        [ test "expand 'ab' returns abc and abd" <|
            \_ ->
                SuffixTrie.expand "ab" trie
                    |> List.map Tuple.first
                    |> List.sort
                    |> Expect.equal [ "abc", "abd" ]
        , test "expand '' returns all entries" <|
            \_ ->
                SuffixTrie.expand "" trie
                    |> List.length
                    |> Expect.equal 3
        , test "expand 'z' returns nothing" <|
            \_ ->
                SuffixTrie.expand "z" trie
                    |> Expect.equal []
        , test "expandIgnoreCase 'AB' returns abc and abd" <|
            \_ ->
                SuffixTrie.expandIgnoreCase "AB" trie
                    |> List.map Tuple.first
                    |> List.sort
                    |> Expect.equal [ "abc", "abd" ]
        , test "isPrefix 'abc' is True for stored key" <|
            \_ ->
                SuffixTrie.isPrefix "abc" trie
                    |> Expect.equal True
        ]


edgeCaseTests : Test
edgeCaseTests =
    describe "Edge cases"
        [ test "empty key can be inserted and retrieved" <|
            \_ ->
                SuffixTrie.singleton "" 42
                    |> SuffixTrie.get ""
                    |> Expect.equal (Just 42)
        , test "single character key" <|
            \_ ->
                SuffixTrie.singleton "a" 1
                    |> SuffixTrie.findSubstring "a"
                    |> Expect.equal [ ( "a", 1 ) ]
        , test "remove cleans up suffixes" <|
            \_ ->
                SuffixTrie.singleton "banana" 1
                    |> SuffixTrie.remove "banana"
                    |> SuffixTrie.isSubstring "an"
                    |> Expect.equal False
        , test "remove followed by insert works correctly" <|
            \_ ->
                SuffixTrie.singleton "banana" 1
                    |> SuffixTrie.remove "banana"
                    |> SuffixTrie.insert "canal" 2
                    |> SuffixTrie.findSubstring "an"
                    |> Expect.equal [ ( "canal", 2 ) ]
        , test "update from Nothing to Just inserts" <|
            \_ ->
                SuffixTrie.empty
                    |> SuffixTrie.update "hello" (\_ -> Just 1)
                    |> SuffixTrie.get "hello"
                    |> Expect.equal (Just 1)
        , test "update from Just to Nothing removes" <|
            \_ ->
                SuffixTrie.singleton "hello" 1
                    |> SuffixTrie.update "hello" (\_ -> Nothing)
                    |> SuffixTrie.isEmpty
                    |> Expect.equal True
        , test "update from Just to Nothing cleans suffixes" <|
            \_ ->
                SuffixTrie.singleton "hello" 1
                    |> SuffixTrie.update "hello" (\_ -> Nothing)
                    |> SuffixTrie.isSubstring "ell"
                    |> Expect.equal False
        , test "update value preserves substring search" <|
            \_ ->
                SuffixTrie.singleton "banana" 1
                    |> SuffixTrie.update "banana" (Maybe.map (\v -> v + 1))
                    |> SuffixTrie.findSubstring "an"
                    |> Expect.equal [ ( "banana", 2 ) ]
        , test "overlapping suffixes with multiple keys" <|
            \_ ->
                SuffixTrie.fromList [ ( "ab", 1 ), ( "cab", 2 ) ]
                    |> SuffixTrie.findSubstring "ab"
                    |> List.map Tuple.first
                    |> List.sort
                    |> Expect.equal [ "ab", "cab" ]
        , test "findSubstring on empty trie returns empty list" <|
            \_ ->
                SuffixTrie.findSubstring "anything" SuffixTrie.empty
                    |> Expect.equal []
        , test "findSuffix on empty trie returns empty list" <|
            \_ ->
                SuffixTrie.findSuffix "anything" SuffixTrie.empty
                    |> Expect.equal []
        , fuzz (list prefixString)
            "Every inserted key is findable as a substring of itself"
            (\vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            st =
                                List.foldl (\v acc -> SuffixTrie.insert v v acc) SuffixTrie.empty vals
                        in
                        Expect.all
                            (List.map
                                (\v t ->
                                    SuffixTrie.findSubstring v t
                                        |> List.map Tuple.first
                                        |> List.member v
                                        |> Expect.equal True
                                )
                                vals
                            )
                            st
            )
        , fuzz (list prefixString)
            "Every inserted key is findable as a suffix of itself"
            (\vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            st =
                                List.foldl (\v acc -> SuffixTrie.insert v v acc) SuffixTrie.empty vals
                        in
                        Expect.all
                            (List.map
                                (\v t ->
                                    SuffixTrie.isSuffix v t
                                        |> Expect.equal True
                                )
                                vals
                            )
                            st
            )
        , fuzz (list prefixString)
            "Remove then isEmpty consistent with size 0"
            (\vals ->
                case vals of
                    [] ->
                        Expect.pass

                    _ ->
                        let
                            st =
                                List.foldl (\v acc -> SuffixTrie.insert v v acc) SuffixTrie.empty vals

                            removed =
                                List.foldl SuffixTrie.remove st vals
                        in
                        removed
                            |> SuffixTrie.isEmpty
                            |> Expect.equal True
            )
        ]
