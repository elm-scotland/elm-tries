module TrieIFace exposing (ITrie)

import DictIface exposing (IDict)
import Trie as Trie exposing (Match, Trie)


type alias ITrie comparable v dict b dictb result comparable1 context =
    IDict comparable
        v
        dict
        b
        dictb
        result
        { match :
            (Maybe comparable1 -> Maybe v -> context -> b -> ( b, context, Match comparable1 ))
            -> b
            -> context
            -> dict
            -> b
        , expand : comparable -> dict -> List ( comparable, v )
        , isSuffix : comparable -> dict -> Bool
        , subtrie : comparable -> dict -> Maybe dict
        }
