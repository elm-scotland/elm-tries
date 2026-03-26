module SuffixTrie exposing
    ( SuffixTrie
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , expand, expandIgnoreCase, isPrefix
    , isSubstring, findSubstring, findSubstringWithPositions, findSubstringIgnoreCase
    , isSuffix, findSuffix
    )

{-| A suffix trie that enables efficient substring searching over stored strings.

It wraps `StringTrie` internally, storing all suffixes of each inserted string
to enable substring lookup. The API mirrors `StringTrie` for Dict-like
operations and adds suffix-trie-specific search functions.


# Data structure

@docs SuffixTrie


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge


# Prefix search

@docs expand, expandIgnoreCase, isPrefix


# Substring search

@docs isSubstring, findSubstring, findSubstringWithPositions, findSubstringIgnoreCase


# Suffix search

@docs isSuffix, findSuffix

-}

import Dict
import StringTrie


{-| An opaque suffix trie type that maps strings to values and supports
efficient substring searching.
-}
type SuffixTrie a
    = SuffixTrie
        { strings : StringTrie.Trie a
        , suffixes : StringTrie.Trie (List { key : String, position : Int })
        }



-- Build


{-| Create an empty suffix trie.
-}
empty : SuffixTrie a
empty =
    SuffixTrie
        { strings = StringTrie.empty
        , suffixes = StringTrie.empty
        }


{-| Create a suffix trie with one key-value pair.
-}
singleton : String -> a -> SuffixTrie a
singleton key val =
    insert key val empty


{-| Insert a key-value pair into a suffix trie. Replaces value when there is
a collision.
-}
insert : String -> a -> SuffixTrie a -> SuffixTrie a
insert key val (SuffixTrie st) =
    SuffixTrie
        { strings = StringTrie.insert key val st.strings
        , suffixes = insertSuffixes key st.suffixes
        }


{-| Update the value of a suffix trie for a specific key with a given function.
-}
update : String -> (Maybe a -> Maybe a) -> SuffixTrie a -> SuffixTrie a
update key fn (SuffixTrie st) =
    let
        oldVal =
            StringTrie.get key st.strings

        newVal =
            fn oldVal
    in
    case ( oldVal, newVal ) of
        ( Nothing, Nothing ) ->
            SuffixTrie st

        ( Nothing, Just val ) ->
            insert key val (SuffixTrie st)

        ( Just _, Nothing ) ->
            remove key (SuffixTrie st)

        ( Just _, Just val ) ->
            SuffixTrie { st | strings = StringTrie.insert key val st.strings }


{-| Remove a key-value pair from a suffix trie. If the key is not found,
no changes are made.
-}
remove : String -> SuffixTrie a -> SuffixTrie a
remove key (SuffixTrie st) =
    if StringTrie.member key st.strings then
        SuffixTrie
            { strings = StringTrie.remove key st.strings
            , suffixes = removeSuffixes key st.suffixes
            }

    else
        SuffixTrie st



-- Query


{-| Determine if a suffix trie is empty.
-}
isEmpty : SuffixTrie a -> Bool
isEmpty (SuffixTrie st) =
    StringTrie.isEmpty st.strings


{-| Determine if a key is in a suffix trie.
-}
member : String -> SuffixTrie a -> Bool
member key (SuffixTrie st) =
    StringTrie.member key st.strings


{-| Get the value associated with a key.
-}
get : String -> SuffixTrie a -> Maybe a
get key (SuffixTrie st) =
    StringTrie.get key st.strings


{-| Determine the number of key-value pairs in the suffix trie.
-}
size : SuffixTrie a -> Int
size (SuffixTrie st) =
    StringTrie.size st.strings



-- Lists


{-| Get all of the keys in a suffix trie, sorted from lowest to highest.
-}
keys : SuffixTrie a -> List String
keys (SuffixTrie st) =
    StringTrie.keys st.strings


{-| Get all of the values in a suffix trie, in the order of their keys.
-}
values : SuffixTrie a -> List a
values (SuffixTrie st) =
    StringTrie.values st.strings


{-| Convert a suffix trie into an association list of key-value pairs, sorted
by keys.
-}
toList : SuffixTrie a -> List ( String, a )
toList (SuffixTrie st) =
    StringTrie.toList st.strings


{-| Convert an association list into a suffix trie.
-}
fromList : List ( String, a ) -> SuffixTrie a
fromList assocs =
    List.foldl (\( key, val ) st -> insert key val st) empty assocs



-- Transform


{-| Apply a function to all values in a suffix trie.
-}
map : (String -> a -> b) -> SuffixTrie a -> SuffixTrie b
map fn (SuffixTrie st) =
    SuffixTrie
        { strings = StringTrie.map fn st.strings
        , suffixes = st.suffixes
        }


{-| Fold over the key-value pairs in a suffix trie from lowest key to highest
key.
-}
foldl : (String -> a -> b -> b) -> b -> SuffixTrie a -> b
foldl fn accum (SuffixTrie st) =
    StringTrie.foldl fn accum st.strings


{-| Fold over the key-value pairs in a suffix trie from highest key to lowest
key.
-}
foldr : (String -> a -> b -> b) -> b -> SuffixTrie a -> b
foldr fn accum (SuffixTrie st) =
    StringTrie.foldr fn accum st.strings


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (String -> a -> Bool) -> SuffixTrie a -> SuffixTrie a
filter isGood st =
    foldl
        (\key val accum ->
            if isGood key val then
                insert key val accum

            else
                accum
        )
        empty
        st


{-| Partition a suffix trie according to some test. The first suffix trie
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (String -> a -> Bool) -> SuffixTrie a -> ( SuffixTrie a, SuffixTrie a )
partition isGood st =
    foldl
        (\key val ( t1, t2 ) ->
            if isGood key val then
                ( insert key val t1, t2 )

            else
                ( t1, insert key val t2 )
        )
        ( empty, empty )
        st



-- Combine


{-| Combine two suffix tries. If there is a collision, preference is given to
the first suffix trie.
-}
union : SuffixTrie a -> SuffixTrie a -> SuffixTrie a
union st1 st2 =
    foldl insert st2 st1


{-| Keep a key-value pair when its key appears in the second suffix trie.
Preference is given to values in the first suffix trie.
-}
intersect : SuffixTrie a -> SuffixTrie a -> SuffixTrie a
intersect st1 st2 =
    filter (\key _ -> member key st2) st1


{-| Keep a key-value pair when its key does not appear in the second suffix
trie.
-}
diff : SuffixTrie a -> SuffixTrie b -> SuffixTrie a
diff st1 st2 =
    foldl
        (\key val accum ->
            if member key st2 then
                accum

            else
                insert key val accum
        )
        empty
        st1


{-| The most general way of combining two suffix tries.
-}
merge :
    (String -> a -> result -> result)
    -> (String -> a -> b -> result -> result)
    -> (String -> b -> result -> result)
    -> SuffixTrie a
    -> SuffixTrie b
    -> result
    -> result
merge leftStep bothStep rightStep (SuffixTrie st1) (SuffixTrie st2) initialResult =
    StringTrie.merge leftStep bothStep rightStep st1.strings st2.strings initialResult



-- Prefix search


{-| Given a prefix, finds all entries where the original key starts with that
prefix.
-}
expand : String -> SuffixTrie a -> List ( String, a )
expand prefix (SuffixTrie st) =
    StringTrie.expand prefix st.strings


{-| Given a prefix, finds all entries where the original key starts with that
prefix, ignoring case.
-}
expandIgnoreCase : String -> SuffixTrie a -> List ( String, a )
expandIgnoreCase prefix (SuffixTrie st) =
    StringTrie.expandIgnoreCase prefix st.strings


{-| Given a prefix, checks if there are keys that begin with that prefix.
-}
isPrefix : String -> SuffixTrie a -> Bool
isPrefix prefix (SuffixTrie st) =
    StringTrie.isPrefix prefix st.strings



-- Substring search


{-| Does any stored key contain the given substring?
-}
isSubstring : String -> SuffixTrie a -> Bool
isSubstring substring (SuffixTrie st) =
    if String.isEmpty substring then
        not (StringTrie.isEmpty st.strings)

    else
        not (List.isEmpty (StringTrie.expand substring st.suffixes))


{-| Returns all (key, value) pairs where the key contains the given substring.
-}
findSubstring : String -> SuffixTrie a -> List ( String, a )
findSubstring substring (SuffixTrie st) =
    if String.isEmpty substring then
        StringTrie.toList st.strings

    else
        let
            suffixHits =
                StringTrie.expand substring st.suffixes

            uniqueKeys =
                suffixHits
                    |> List.concatMap Tuple.second
                    |> List.map .key
                    |> uniqueStrings
        in
        uniqueKeys
            |> List.filterMap
                (\key ->
                    StringTrie.get key st.strings
                        |> Maybe.map (\val -> ( key, val ))
                )


{-| Returns all (key, value, positions) triples where the key contains the
given substring, along with the character positions where the substring occurs.
-}
findSubstringWithPositions : String -> SuffixTrie a -> List ( String, a, List Int )
findSubstringWithPositions substring (SuffixTrie st) =
    if String.isEmpty substring then
        StringTrie.toList st.strings
            |> List.map
                (\( key, val ) ->
                    ( key, val, List.range 0 (String.length key) )
                )

    else
        let
            suffixHits =
                StringTrie.expand substring st.suffixes

            keyPositions =
                suffixHits
                    |> List.concatMap Tuple.second
                    |> List.foldl
                        (\entry acc ->
                            Dict.update entry.key
                                (\existing ->
                                    case existing of
                                        Nothing ->
                                            Just [ entry.position ]

                                        Just positions ->
                                            Just (entry.position :: positions)
                                )
                                acc
                        )
                        Dict.empty
        in
        Dict.toList keyPositions
            |> List.filterMap
                (\( key, positions ) ->
                    StringTrie.get key st.strings
                        |> Maybe.map
                            (\val ->
                                ( key, val, List.sort positions |> uniqueInts )
                            )
                )


{-| Case-insensitive variant of `findSubstring`.
-}
findSubstringIgnoreCase : String -> SuffixTrie a -> List ( String, a )
findSubstringIgnoreCase substring (SuffixTrie st) =
    if String.isEmpty substring then
        StringTrie.toList st.strings

    else
        let
            suffixHits =
                StringTrie.expandIgnoreCase substring st.suffixes

            uniqueKeys =
                suffixHits
                    |> List.concatMap Tuple.second
                    |> List.map .key
                    |> uniqueStrings
        in
        uniqueKeys
            |> List.filterMap
                (\key ->
                    StringTrie.get key st.strings
                        |> Maybe.map (\val -> ( key, val ))
                )



-- Suffix search


{-| Is the given string a suffix of any stored key?
-}
isSuffix : String -> SuffixTrie a -> Bool
isSuffix suffix (SuffixTrie st) =
    if String.isEmpty suffix then
        not (StringTrie.isEmpty st.strings)

    else
        case StringTrie.get suffix st.suffixes of
            Nothing ->
                False

            Just entries ->
                List.any
                    (\entry ->
                        entry.position + String.length suffix == String.length entry.key
                    )
                    entries


{-| Returns all (key, value) pairs where the key ends with the given suffix.
-}
findSuffix : String -> SuffixTrie a -> List ( String, a )
findSuffix suffix (SuffixTrie st) =
    if String.isEmpty suffix then
        StringTrie.toList st.strings

    else
        case StringTrie.get suffix st.suffixes of
            Nothing ->
                []

            Just entries ->
                entries
                    |> List.filter
                        (\entry ->
                            entry.position + String.length suffix == String.length entry.key
                        )
                    |> List.map .key
                    |> uniqueStrings
                    |> List.filterMap
                        (\key ->
                            StringTrie.get key st.strings
                                |> Maybe.map (\val -> ( key, val ))
                        )



-- Internal helpers


insertSuffixes :
    String
    -> StringTrie.Trie (List { key : String, position : Int })
    -> StringTrie.Trie (List { key : String, position : Int })
insertSuffixes key suffixTrie =
    let
        keyLen =
            String.length key
    in
    List.range 0 (keyLen - 1)
        |> List.foldl
            (\pos acc ->
                let
                    suffix =
                        String.dropLeft pos key

                    entry =
                        { key = key, position = pos }
                in
                StringTrie.update suffix
                    (\existing ->
                        case existing of
                            Nothing ->
                                Just [ entry ]

                            Just entries ->
                                Just (entry :: entries)
                    )
                    acc
            )
            suffixTrie


removeSuffixes :
    String
    -> StringTrie.Trie (List { key : String, position : Int })
    -> StringTrie.Trie (List { key : String, position : Int })
removeSuffixes key suffixTrie =
    let
        keyLen =
            String.length key
    in
    List.range 0 (keyLen - 1)
        |> List.foldl
            (\pos acc ->
                let
                    suffix =
                        String.dropLeft pos key
                in
                StringTrie.update suffix
                    (\existing ->
                        case existing of
                            Nothing ->
                                Nothing

                            Just entries ->
                                case List.filter (\e -> e.key /= key) entries of
                                    [] ->
                                        Nothing

                                    remaining ->
                                        Just remaining
                    )
                    acc
            )
            suffixTrie


uniqueStrings : List String -> List String
uniqueStrings list =
    list
        |> List.foldl
            (\item ( seen, acc ) ->
                if List.member item seen then
                    ( seen, acc )

                else
                    ( item :: seen, item :: acc )
            )
            ( [], [] )
        |> Tuple.second
        |> List.reverse


uniqueInts : List Int -> List Int
uniqueInts list =
    list
        |> List.foldl
            (\item ( seen, acc ) ->
                if List.member item seen then
                    ( seen, acc )

                else
                    ( item :: seen, item :: acc )
            )
            ( [], [] )
        |> Tuple.second
        |> List.reverse
