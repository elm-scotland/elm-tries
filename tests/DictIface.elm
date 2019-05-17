module DictIface exposing
    ( IDict
    , listOfValsContainsAllVals
    )

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Fuzzers exposing (..)
import Set
import Test exposing (..)
import Trie exposing (Trie)


type alias IDict comparable v dict =
    { empty : dict
    , singleton : comparable -> v -> dict
    , get : comparable -> dict -> Maybe v
    , insert : comparable -> v -> dict -> dict
    }



-- dict : IDict comparable v (Dict comparable v)
-- dict =
--     { empty = Dict.empty
--     , singleton = Dict.singleton
--     , get = Dict.get
--     , insert = Dict.insert
--     }


listOfValsContainsAllVals fuzzName implName fuzzer dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> Expect.all (List.map (\val trie -> dictImpl.get val trie |> Expect.equal (Just val)) vals)
    in
    fuzz fuzzer
        ("Creates a " ++ implName ++ " with a list of vals and ensures it contains all of them (" ++ fuzzName ++ ").")
        test
