module Fuzzers exposing (longString, suffixString)

import Fuzz exposing (Fuzzer, int, list, string)



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


longString factor =
    Fuzz.frequency
        [ ( 1, Fuzz.constant (String.repeat factor "a") )
        , ( 1, Fuzz.constant (String.repeat factor "b") )
        , ( 1, Fuzz.constant (String.repeat factor "c") )
        , ( 1, Fuzz.constant (String.repeat factor "d") )
        ]
        |> Fuzz.list
        |> Fuzz.map String.concat
