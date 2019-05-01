module Trie exposing (Trie(..))


type Trie a
    = Trie


empty : Trie a
empty =
    Trie


singleton : String -> a -> Trie a
singleton _ _ =
    Debug.todo "singleton"


insert : String -> a -> Trie a -> Trie a
insert _ _ _ =
    Debug.todo "insert"


update : String -> (Maybe a -> Maybe a) -> Trie a -> Trie a
update _ _ _ =
    Debug.todo "update"


remove : String -> Trie a -> Trie a
remove _ _ =
    Debug.todo "remove"


isEmpty : Trie a -> Bool
isEmpty _ =
    Debug.todo "isEmpty"


member : String -> Trie a -> Bool
member _ _ =
    Debug.todo "member"


get : String -> Trie a -> Maybe a
get _ _ =
    Debug.todo "get"


size : Trie a -> Int
size _ =
    Debug.todo "size"


keys : Trie a -> List String
keys _ =
    Debug.todo "keys"


values : Trie a -> List a
values _ =
    Debug.todo "values"


toList : Trie a -> List ( String, a )
toList _ =
    Debug.todo "toList"


fromList : List ( String, a ) -> Trie a
fromList _ =
    Debug.todo "fromList"


map : (String -> a -> b) -> Trie a -> Trie b
map _ _ =
    Debug.todo "map"


foldl : (String -> a -> b -> b) -> b -> Trie a -> b
foldl _ _ _ =
    Debug.todo "foldl"


foldr : (String -> a -> b -> b) -> b -> Trie a -> b
foldr _ _ _ =
    Debug.todo "foldr"


filter : (String -> a -> Bool) -> Trie a -> Trie a
filter _ _ =
    Debug.todo "filter"


partition : (String -> a -> Bool) -> Trie a -> ( Trie a, Trie a )
partition _ _ =
    Debug.todo "partition"


union : Trie a -> Trie a -> Trie a
union _ _ =
    Debug.todo "union"


intersect : Trie a -> Trie a -> Trie a
intersect _ _ =
    Debug.todo "intersect"


diff : Trie a -> Trie b -> Trie a
diff _ _ =
    Debug.todo "diff"


merge :
    (String -> a -> result -> result)
    -> (String -> a -> b -> result -> result)
    -> (String -> b -> result -> result)
    -> Trie a
    -> Trie b
    -> result
    -> result
merge _ _ _ _ _ _ =
    Debug.todo "merge"


expand : String -> Trie a -> List String
expand _ _ =
    Debug.todo "expand"


matches : String -> Trie a -> Bool
matches _ _ =
    Debug.todo "matches"


subtrie : String -> Trie a -> Maybe (Trie a)
subtrie _ _ =
    Debug.todo "subtrie"



-- Ideas
-- expandIgnoreCase : String -> Trie a -> List String
-- matchesIgnoreCase : String -> Trie a -> Bool
-- match : (List Char -> Char -> a -> Bool) -> Trie a -> Bool
-- match _ _ =
--     Debug.todo "map"
