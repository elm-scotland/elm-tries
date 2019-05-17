module DictIface exposing (listOfValsContainsAllVals)

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


test1 : Test
test1 =
    listOfValsContainsAllVals trieDict


test2 : Test
test2 =
    listOfValsContainsAllVals dict


listOfValsContainsAllVals : IDict String String dict -> Test
listOfValsContainsAllVals dictImpl =
    let
        test possiblyEmptyVals =
            case possiblyEmptyVals of
                [] ->
                    Expect.equal [] []

                vals ->
                    List.foldl (\val trie -> dictImpl.insert val val trie) dictImpl.empty vals
                        |> Expect.all (List.map (\val trie -> dictImpl.get val trie |> Expect.equal (Just val)) vals)
    in
    describe "listOfValsContainsAllVals"
        [ fuzz (list string)
            "Creates a trie with a list of vals and ensures it contains all of them (list string)."
            test
        , fuzz (list suffixString)
            "Creates a trie with a list of vals and ensures it contains all of them (list suffixString)."
            test
        ]
