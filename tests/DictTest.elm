module DictTest exposing (suite)

import Dict exposing (Dict)
import DictIface exposing (IDict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Fuzzers exposing (..)
import Set
import Test exposing (..)


dict : IDict comparable v (Dict comparable v)
dict =
    { empty = Dict.empty
    , singleton = Dict.singleton
    , get = Dict.get
    , insert = Dict.insert
    }


suite : Test
suite =
    describe "Trie tests"
        [ DictIface.listOfValsContainsAllVals "list string" "dict" (list string) dict
        , DictIface.listOfValsContainsAllVals "list suffixString" "dict" (list suffixString) dict
        ]
