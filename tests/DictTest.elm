module DictTest exposing (suite)

import Dict exposing (Dict)
import DictIface exposing (IDict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Fuzzers exposing (..)
import Set
import Test exposing (..)


dict : IDict comparable v (Dict comparable v) b
dict =
    { empty = Dict.empty
    , singleton = Dict.singleton
    , insert = Dict.insert
    , update = Dict.update
    , remove = Dict.remove
    , isEmpty = Dict.isEmpty
    , member = Dict.member
    , get = Dict.get
    , size = Dict.size
    , keys = Dict.keys
    , values = Dict.values
    , toList = Dict.toList
    , fromList = Dict.fromList

    --, map =  Dict.map
    , foldl = Dict.foldl
    , foldr = Dict.foldr
    , filter = Dict.filter
    , partition = Dict.partition
    , union = Dict.union
    , intersect = Dict.intersect
    , diff = Dict.diff
    }


suite : Test
suite =
    describe "Dict tests"
        [ DictIface.emptyIsEmpty "dict" dict ]



-- [ DictIface.listOfValsContainsAllVals "list string" "dict" (list string) dict
-- , DictIface.listOfValsContainsAllVals "list suffixString" "dict" (list suffixString) dict
-- ]
