module DictTest exposing (suite)

import Dict exposing (Dict)
import DictIface exposing (IDict)
import Fuzz exposing (int, list, string)
import Fuzzers exposing (longString, prefixString)
import Test exposing (Test, describe)


dict : IDict comparable v (Dict comparable v) b (Dict comparable b) result {}
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
    , map = Dict.map
    , foldl = Dict.foldl
    , foldr = Dict.foldr
    , filter = Dict.filter
    , partition = Dict.partition
    , union = Dict.union
    , intersect = Dict.intersect
    , diff = Dict.diff
    , merge = Dict.merge
    }


suite : Test
suite =
    describe "Dict tests"
        [ DictIface.emptyIsEmpty " dict" dict
        , DictIface.emptyContainsNoVal "string" " dict" string dict
        , DictIface.emptyInsertStringContainsVal "string" " dict" string dict
        , DictIface.nonEmptyIsNotEmpty "list string" " dict" (list string) dict
        , DictIface.singletonContainsVal "string" " dict" string dict
        , DictIface.singletonEmptyStringContainsVal "string" " dict" string dict
        , DictIface.listOfNumsDoubledAllEven "list int" " dict" (list int) dict
        , DictIface.listOfValsAllKeysMembers "list string" " dict" (list string) dict
        , DictIface.listOfValsContainsAllVals "list string" " dict" (list string) dict
        , DictIface.listOfValsFoldlAllKeys "list string" " dict" (list string) dict
        , DictIface.listOfValsFoldlIncreasing "list string" " dict" (list string) dict
        , DictIface.listOfValsFoldrAllKeys "list string" " dict" (list string) dict
        , DictIface.listOfValsFoldrDecreasing "list string" " dict" (list string) dict
        , DictIface.listOfValsListsAllKeys "list string" " dict" (list string) dict
        , DictIface.listOfValsListsAllValues "list string" " dict" (list string) dict
        , DictIface.listOfValsRemovedContainsNone "list string" " dict" (list string) dict
        , DictIface.listOfValsReportsSizeOk "list string" " dict" (list string) dict
        , DictIface.listOfValsReportsSizeOk "list <| longString 10" " dict" (list <| longString 10) dict
        ]
