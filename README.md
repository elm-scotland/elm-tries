# elm-tries

A trie is a data structure similar to a `Dict` or associative mapping. The keys
in a trie are often `String`s but this Elm implementation also provides tries
with `List comparable` keys.

The trie API implemented here is a super-set of the `Dict` API from `elm/core`;
so long as your keys are `String` or `List comparable` that is.

The difference between a trie and an associative mapping, is that the keys
are lists and multiple keys that share lists as common prefixes share space in
the data structure for the part that is common. This can make searching strings
efficient in time and space; time because multiple keys can be explored at the
same time; space because of the sharing of the common prefixes.

Tries are used in many efficient string searching algorithms.
