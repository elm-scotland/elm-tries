module TrieIface exposing
    ( ITrie
    , expandTest
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
                |> Expect.false "key does not expland to itself"

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
