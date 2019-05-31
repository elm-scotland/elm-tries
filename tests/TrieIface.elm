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
        , isSuffix : comparable -> dict -> Bool
        , subtrie : comparable -> dict -> Maybe dict
        }


expandTest :
    String
    -> String
    -> Fuzzer (List comparable)
    -> ITrie comparable comparable dict b dictb result comparable1 context
    -> Test
expandTest fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> (\trie -> Expect.all (List.map (\val list -> List.isEmpty (dictImpl.expand val trie |> Debug.log "expand") |> Expect.false "key does not expland to itself") vals) trie)

        -- Every key suffix expands to some results.
        -- The results all have the expanded suffix as a suffix
        -- k-v pairs in the results match up
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with ..., checking ... (" ++ fuzzName ++ ").")
        test
