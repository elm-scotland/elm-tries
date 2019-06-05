module StringTrie exposing
    ( Trie
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , Match, match, expand, isSuffix, subtrie
    , expandIgnoreCase
    )

{-| A trie mapping unique strings to values.


# Data structure

@docs Trie


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


# Trie specific search operations

@docs Match, match, expand, isSuffix, subtrie


# String specific operations

@docs expandIgnoreCase

-}

import Dict exposing (Dict)
import Trie exposing (Match(..))


{-| A trie mapping keys to values, where the keys are `String`.

This version of `Trie` is a lot like a `Dict` except the keys are must be
strings. Keys that have common suffixes share space, and it is possible to
efficiently search for keys matching a particular suffix.

-}
type alias Trie a =
    Trie.Trie Char a


{-| Create an empty trie.
-}
empty : Trie a
empty =
    Trie.empty


{-| Create a trie with one key-value pair.
-}
singleton : String -> a -> Trie a
singleton key val =
    Trie.singleton (String.toList key) val


{-| Insert a key-value pair into a trie. Replaces value when there is
a collision.
-}
insert : String -> a -> Trie a -> Trie a
insert key val trie =
    Trie.insert (String.toList key) val trie


{-| Update the value of a trie for a specific key with a given function.
-}
update : String -> (Maybe a -> Maybe a) -> Trie a -> Trie a
update key fn trie =
    Trie.update (String.toList key) fn trie


{-| Remove a key-value pair from a trie. If the key is not found,
no changes are made.
-}
remove : String -> Trie a -> Trie a
remove key trie =
    Trie.remove (String.toList key) trie


{-| Determine if a trie is empty.

    `isEmpty empty == True`

-}
isEmpty : Trie a -> Bool
isEmpty trie =
    Trie.isEmpty trie


{-| Determine if a whole key is in a trie.
-}
member : String -> Trie a -> Bool
member key trie =
    Trie.member (String.toList key) trie


{-| Get the value associated with a key. If the key is not found, return
`Nothing`.
-}
get : String -> Trie a -> Maybe a
get key trie =
    Trie.get (String.toList key) trie


{-| Determine the number of key-value pairs in the trie.
-}
size : Trie a -> Int
size trie =
    Trie.size trie


{-| Get all of the keys in a trie, sorted from lowest to highest.
-}
keys : Trie a -> List String
keys trie =
    Trie.keys trie
        |> List.map String.fromList


{-| Get all of the values in a trie, in the order of their keys.
-}
values : Trie a -> List a
values trie =
    Trie.values trie


{-| Convert a trie into an association list of key-value pairs, sorted by keys.
-}
toList : Trie a -> List ( String, a )
toList trie =
    Trie.toList trie
        |> List.map (Tuple.mapFirst String.fromList)


{-| Convert an association list into a trie.
-}
fromList : List ( String, a ) -> Trie a
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs


{-| Apply a function to all values in a trie.
-}
map : (String -> a -> b) -> Trie a -> Trie b
map fn trie =
    Trie.map (\chars -> fn <| String.fromList chars) trie


{-| Fold over the key-value pairs in a trie from lowest key to highest key.
-}
foldl : (String -> a -> b -> b) -> b -> Trie a -> b
foldl fn accum trie =
    Trie.foldl (\chars -> fn <| String.fromList chars) accum trie


{-| Fold over the key-value pairs in a trie from highest key to lowest key.

Due to the way shorter keys are nearer the top of the trie this fold function
has to hold more pending nodes in memory in order to fold in order from the
highest key to the lowest key. For this reason it is less efficient than `foldl`
and `foldl` should be preferred unless the ordering is important.

-}
foldr : (String -> a -> b -> b) -> b -> Trie a -> b
foldr fn accum trie =
    Trie.foldr (\chars -> fn <| String.fromList chars) accum trie


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (String -> a -> Bool) -> Trie a -> Trie a
filter isGood trie =
    Trie.filter (\chars -> isGood <| String.fromList chars) trie


{-| Partition a trie according to some test. The first trie contains all
key-value pairs which passed the test, and the second contains the pairs that
did not.
-}
partition : (String -> a -> Bool) -> Trie a -> ( Trie a, Trie a )
partition isGood trie =
    Trie.partition (\chars -> isGood <| String.fromList chars) trie


{-| Combine two tries. If there is a collision, preference is given to the first
trie.
-}
union : Trie a -> Trie a -> Trie a
union t1 t2 =
    Trie.union t1 t2


{-| Keep a key-value pair when its key appears in the second trie. Preference is
given to values in the first dictionary.
-}
intersect : Trie a -> Trie a -> Trie a
intersect t1 t2 =
    Trie.intersect t1 t2


{-| Keep a key-value pair when its key does not appear in the second trie.
-}
diff : Trie a -> Trie b -> Trie a
diff t1 t2 =
    Trie.diff t1 t2


{-| The most general way of combining two tries. You provide three accumulators
for when a given key appears:

1.  Only in the left trie.
2.  In both tries.
3.  Only in the right trie.

You then traverse all the keys from lowest to highest, building up whatever you
want.

-}
merge :
    (String -> a -> result -> result)
    -> (String -> a -> b -> result -> result)
    -> (String -> b -> result -> result)
    -> Trie a
    -> Trie b
    -> result
    -> result
merge leftStep bothStep rightStep leftTrie rightTrie initialResult =
    Trie.merge
        (\chars -> leftStep <| String.fromList chars)
        (\chars -> bothStep <| String.fromList chars)
        (\chars -> rightStep <| String.fromList chars)
        leftTrie
        rightTrie
        initialResult


{-| Given a suffix, finds all keys that begin with that suffix.
-}
expand : String -> Trie a -> List ( String, a )
expand key trie =
    Trie.expand (String.toList key) trie
        |> List.map (Tuple.mapFirst String.fromList)


{-| Given a suffix, checks if there are keys that begin with that suffix.
-}
isSuffix : String -> Trie a -> Bool
isSuffix key trie =
    Trie.isSuffix (String.toList key) trie


{-| Given a suffix, finds any sub-trie containing the key-value pairs where the
original keys begin with that suffix. The keys in the sub-trie will only consist
of the remaining portion of the key after the suffix.
-}
subtrie : String -> Trie a -> Maybe (Trie a)
subtrie key trie =
    Trie.subtrie (String.toList key) trie


{-| `Match` describes how a flexible search over a trie will proceed.

  - `Break` - do not explore any more below the current suffix.
  - `Wildcard` - continue with all possible next keys below the current suffix.
  - `ContinueIf` - continue with the next key provided it exactly matches the comparable specified.
  - `ContinueIfOneOf` - continue with the next key provided it matches one of the comparables specified.

The `Break`, `ContinueIf` and `ContinueIfOneOf` options allow a trie to be
traversed efficiently without exploring unnecessary keys.

The `Wildcard` and `ContinueIfOneOf` options allow flexible matching within a
trie. Functions such as case-insensitive matching, fuzzy matching or regular
expression matching can be implemented using these options.

-}
type alias Match comparable =
    Trie.Match comparable


{-| Performs a flexible matching fold over a trie from the lowest to the highest
key in order.

Suppose the function passed in has this form:

`searchFn maybeKeyPart maybeValue context accum = ...`

The `maybeKeyPart` parameter will be set to the next item from the key being
scanned as a list. This is a `Maybe` as the empty list can be a key in a trie.
In practice the value `Nothing` will only be passed to this function on the
first call when the empty key is present.

The `maybeValue` parameter will be set to any value found at the current position
in the trie.

The `context` parameter will be held against the particular node in the trie
being explored. When and if that node is returned to in order to explore other
key paths in the trie, the context for that node will be restored. The trie is
explored using a depth first search, and the contexts are held in a stack of
pending nodes to explore. An example use of the context might be to hold the
remaining portion of a key to be matched.

The `accum` parameter is used like the accumulator in a fold, it can be updated
on each node explored.

The `context` parameter is restored when back-tracking to explore other possible
keys, but the `accum` parameter is carried accross the whole search. In that
sense `context` is like a local variable and `accum` is like a global variable.

-}
match :
    (Maybe Char -> Maybe a -> context -> b -> ( b, context, Match Char ))
    -> b
    -> context
    -> Trie a
    -> b
match fn accum context trie =
    Trie.match fn accum context trie



-- String specific Trie functions.


{-| Given a suffix, finds all keys that begin with that suffix ignoring the case
of characters in the suffix or in the trie.
-}
expandIgnoreCase : String -> Trie a -> List ( String, a )
expandIgnoreCase key trie =
    match
        (\maybeChar maybeValue ( remainingKey, matchedKey ) accum ->
            case remainingKey of
                [] ->
                    case maybeValue of
                        Nothing ->
                            ( accum, ( [], matchedKey ), Break )

                        Just val ->
                            ( ( String.fromList (List.reverse matchedKey), val ) :: accum, ( [], matchedKey ), Break )

                head :: nextRemainingKey ->
                    let
                        nextMatchedKey =
                            case maybeChar of
                                Nothing ->
                                    matchedKey

                                Just c ->
                                    c :: matchedKey

                        nextMatch =
                            if Char.toLower head == Char.toUpper head then
                                ContinueIf head

                            else
                                ContinueIfOneOf [ Char.toLower head, Char.toUpper head ]

                        nextAccum =
                            case maybeValue of
                                Nothing ->
                                    accum

                                Just val ->
                                    ( String.fromList (List.reverse nextMatchedKey), val ) :: accum
                    in
                    ( nextAccum, ( nextRemainingKey, nextMatchedKey ), nextMatch )
        )
        []
        ( String.toList key, [] )
        trie
