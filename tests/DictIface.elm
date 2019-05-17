module DictIface exposing (suite)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Set
import Test exposing (..)
import Trie exposing (Trie)


type alias IDict comparable v dict =
    { empty : dict
    , singleton : comparable -> v -> dict
    , get : comparable -> dict -> Maybe v
    , insert : comparable -> v -> dict -> dict
    }


trieDict : IDict String a (Trie.Trie a)
trieDict =
    { empty = Trie.empty
    , singleton = Trie.singleton
    , get = Trie.get
    , insert = Trie.insert
    }


dict : IDict comparable v (Dict comparable v)
dict =
    { empty = Dict.empty
    , singleton = Dict.singleton
    , get = Dict.get
    , insert = Dict.insert
    }



-- Fuzzers targeted at trie testing.


suffixString =
    Fuzz.frequency
        [ ( 4, Fuzz.constant "a" )
        , ( 3, Fuzz.constant "b" )
        , ( 2, Fuzz.constant "c" )
        , ( 1, Fuzz.constant "d" )
        , ( 1, Fuzz.constant "" )
        ]
        |> Fuzz.list
        |> Fuzz.map (String.concat >> String.left 10)


suite =
    describe "listOfValsContainsAllVals"
        [ fuzz (list string)
            "Creates a trie with a list of vals and ensures it contains all of them (list string)."
            (listOfValsContainsAllVals trieDict)
        , fuzz (list suffixString)
            "Creates a trie with a list of vals and ensures it contains all of them (list suffixString)."
            (listOfValsContainsAllVals trieDict)
        , fuzz (list string)
            "Creates a dict with a list of vals and ensures it contains all of them (list string)."
            (listOfValsContainsAllVals dict)
        , fuzz (list suffixString)
            "Creates a dict with a list of vals and ensures it contains all of them (list suffixString)."
            (listOfValsContainsAllVals dict)
        ]


listOfValsContainsAllVals dictImpl possiblyEmptyVals =
    case possiblyEmptyVals of
        [] ->
            Expect.equal [] []

        vals ->
            List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                |> Expect.all (List.map (\val trie -> dictImpl.get val trie |> Expect.equal (Just val)) vals)
