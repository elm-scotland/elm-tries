module TrieIface exposing
    ( ITrie
    , expandEmptyPrefixReturnsAll
    , expandResultsHavePrefix
    , expandTest
    , isPrefixOfInsertedKey
    , subtrieEmptyKeyReturnsSelf
    , subtrieOfInsertedKeyHasValue
    )

import DictIface exposing (IDict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Set exposing (Set)
import Test exposing (Test, fuzz)
import Trie as Trie exposing (Match, Trie)


type alias ITrie comparable v dict b dictb result comparable1 context =
    IDict comparable
        v
        dict
        b
        dictb
        result
        { match :
            (Maybe comparable1 -> Maybe v -> context -> b -> ( b, context, Match comparable1 ))
            -> b
            -> context
            -> dict
            -> b
        , expand : comparable -> dict -> List ( comparable, v )
        , isPrefix : comparable -> dict -> Bool
        , subtrie : comparable -> dict -> Maybe dict
        }


expandTest :
    String
    -> String
    -> Fuzzer (List comparable)
    -> ITrie comparable comparable dict b dictb result comparable1 context
    -> Test
expandTest fuzzName implName fuzzer trieImpl =
    let
        checkVals fn vals =
            List.map fn vals

        -- Every key expands to some results.
        expectKeyExpand val trie =
            List.isEmpty (trieImpl.expand val trie)
                |> Expect.equal False

        -- Every key prefix expands to some results.
        -- expectAllKeyPrefixesExpand val trie =
        --     Expect.pass
        -- The results all have the expanded prefix as a prefix
        --
        -- expectKeysHaveExpandedPrefix val trie =
        --     let
        --         result =
        --             trieImpl.expand val trie
        --     in
        --     Expect.all (List.map (\( k, v ) -> \sfx -> Expect.pass) result) val
        --
        -- k-v pairs in the results match up
        expectKvPairs val trie =
            let
                result =
                    trieImpl.expand val trie
            in
            Expect.all (List.map (\( k, v ) -> \() -> Expect.equal k v) result) ()

        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> trieImpl.insert val val trie) trieImpl.empty vals
                        |> (\trie ->
                                Expect.all
                                    (checkVals expectKeyExpand vals
                                        ++ checkVals expectKvPairs vals
                                    )
                                    trie
                           )
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals, checking expansion of keys and prefixes of them (" ++ fuzzName ++ ").")
        test


expandResultsHavePrefix :
    String
    -> String
    -> Fuzzer (List comparable)
    -> (comparable -> comparable -> Bool)
    -> ITrie comparable comparable dict b dictb result comparable1 context
    -> Test
expandResultsHavePrefix fuzzName implName fuzzer startsWith trieImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        trie =
                            List.foldl (\val t -> trieImpl.insert val val t) trieImpl.empty vals
                    in
                    Expect.all
                        (List.map
                            (\val t ->
                                let
                                    results =
                                        trieImpl.expand val t
                                in
                                List.all (\( k, _ ) -> startsWith val k) results
                                    |> Expect.equal True
                            )
                            vals
                        )
                        trie
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " and checks expand results have the correct prefix (" ++ fuzzName ++ ").")
        test


expandEmptyPrefixReturnsAll :
    String
    -> String
    -> Fuzzer (List comparable)
    -> comparable
    -> ITrie comparable comparable dict b dictb result comparable1 context
    -> Test
expandEmptyPrefixReturnsAll fuzzName implName fuzzer emptyKey trieImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        trie =
                            List.foldl (\val t -> trieImpl.insert val val t) trieImpl.empty vals
                    in
                    trieImpl.expand emptyKey trie
                        |> List.length
                        |> Expect.equal (trieImpl.size trie)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " and checks expand with empty key returns all entries (" ++ fuzzName ++ ").")
        test


subtrieEmptyKeyReturnsSelf :
    String
    -> String
    -> Fuzzer (List comparable)
    -> comparable
    -> ITrie comparable comparable dict b dictb result comparable1 context
    -> Test
subtrieEmptyKeyReturnsSelf fuzzName implName fuzzer emptyKey trieImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        trie =
                            List.foldl (\val t -> trieImpl.insert val val t) trieImpl.empty vals
                    in
                    case trieImpl.subtrie emptyKey trie of
                        Nothing ->
                            Expect.fail "subtrie with empty key returned Nothing"

                        Just sub ->
                            trieImpl.size sub |> Expect.equal (trieImpl.size trie)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " and checks subtrie with empty key returns the whole trie (" ++ fuzzName ++ ").")
        test


subtrieOfInsertedKeyHasValue :
    String
    -> String
    -> Fuzzer (List comparable)
    -> comparable
    -> ITrie comparable comparable dict b dictb result comparable1 context
    -> Test
subtrieOfInsertedKeyHasValue fuzzName implName fuzzer emptyKey trieImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        trie =
                            List.foldl (\val t -> trieImpl.insert val val t) trieImpl.empty vals
                    in
                    Expect.all
                        (List.map
                            (\val t ->
                                case trieImpl.subtrie val t of
                                    Nothing ->
                                        Expect.fail "subtrie returned Nothing for inserted key"

                                    Just sub ->
                                        trieImpl.get emptyKey sub |> Expect.equal (Just val)
                            )
                            vals
                        )
                        trie
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " and checks subtrie of each key has the value at empty key (" ++ fuzzName ++ ").")
        test


isPrefixOfInsertedKey :
    String
    -> String
    -> Fuzzer (List comparable)
    -> ITrie comparable comparable dict b dictb result comparable1 context
    -> Test
isPrefixOfInsertedKey fuzzName implName fuzzer trieImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    let
                        trie =
                            List.foldl (\val t -> trieImpl.insert val val t) trieImpl.empty vals
                    in
                    Expect.all
                        (List.map
                            (\val t ->
                                trieImpl.isPrefix val t |> Expect.equal True
                            )
                            vals
                        )
                        trie
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " and checks isPrefix returns True for each inserted key (" ++ fuzzName ++ ").")
        test
