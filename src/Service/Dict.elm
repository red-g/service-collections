module Service.Dict exposing (Dict, max, foldl, foldr, fromList, get, isEmpty, previous, member, middle, next, remove, min, filter, size, insert, keys, values, map, update, singleton, toList)

{-| `Service.Dict`s provide the exact same functionality as Elm's built-in `Dict` type, while allowing for easy and safe extension with custom key types.

@docs Dict, max, foldl, foldr, fromList, get, isEmpty, previous, member, middle, next, remove, min, filter, size, insert, keys, values, map, update, singleton, toList

    module MyRecordDict exposing (MyRecordDict, fromList, get {- , ... all the other implementations -})

    import Service.Dict as Dict exposing (Dict)
    import Sort exposing (Sorter)

    type MyRecordDict v
        = MyRecordDict (Dict MyRecord v)

    type alias MyRecord =
        { foo : Int, bar : String }

    sorter : Sorter MyRecord
    sorter =
        Sort.by .foo Sort.int
            |> Sort.and (Sort.by .bar Sort.string)

    fromList : List ( MyRecord, v ) -> MyRecordDict v
    fromList =
        Dict.fromList sorter

    get : MyRecord -> MyRecordDict v -> Maybe v
    get record (MyRecordDict dict) =
        Dict.get sorter record dict

    -- ... all the other implementations

-}

import Filter exposing (Filter)
import Sort exposing (Sorter)


type Color
    = Red
    | Black


{-| A mapping of unique keys to values.
-}
type Dict k v
    = Node Color k v (Dict k v) (Dict k v)
    | Leaf


{-| Get the middle key-value pair of the dictionary.
-}
middle : Dict k v -> Maybe ( k, v )
middle dict =
    case dict of
        Node _ key value _ _ ->
            Just ( key, value )

        Leaf ->
            Nothing


{-| An empty dictionary.
-}
empty : Dict k v
empty =
    Leaf


{-| Get the value of a key in the dictionary.
-}
get : Sorter k -> k -> Dict k v -> Maybe v
get sorter key dict =
    getHelper (Sort.order sorter key) dict


getHelper : (k -> Order) -> Dict k v -> Maybe v
getHelper sorter dict =
    case dict of
        Leaf ->
            Nothing

        Node _ key value left right ->
            case sorter key of
                EQ ->
                    Just value

                LT ->
                    getHelper sorter left

                GT ->
                    getHelper sorter right


{-| Check if the given key exists in the dictionary.
-}
member : Sorter k -> k -> Dict k v -> Bool
member sorter key dict =
    memberHelper (Sort.order sorter key) dict


memberHelper : (k -> Order) -> Dict k v -> Bool
memberHelper sorter dict =
    case dict of
        Leaf ->
            False

        Node _ key _ gt right ->
            case sorter key of
                EQ ->
                    True

                LT ->
                    memberHelper sorter gt

                GT ->
                    memberHelper sorter right


{-| Check if the dictionary is empty.
-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    case dict of
        Leaf ->
            True

        _ ->
            False


{-| Insert a value at the given key.
-}
insert : Sorter k -> k -> v -> Dict k v -> Dict k v
insert sorter key value dict =
    -- Root node is always Black
    case insertHelp sorter key value dict of
        Node Red k v l r ->
            Node Black k v l r

        x ->
            x


insertHelp : Sorter k -> k -> v -> Dict k v -> Dict k v
insertHelp sorter key value dict =
    case dict of
        Leaf ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            Node Red key value Leaf Leaf

        Node nColor nKey nValue nLeft nRight ->
            case Sort.order sorter key nKey of
                LT ->
                    balance nColor nKey nValue (insertHelp sorter key value nLeft) nRight

                EQ ->
                    Node nColor nKey value nLeft nRight

                GT ->
                    balance nColor nKey nValue nLeft (insertHelp sorter key value nRight)


balance : Color -> k -> v -> Dict k v -> Dict k v -> Dict k v
balance color key value left right =
    case right of
        Node Red rK rV rLeft rRight ->
            case left of
                Node Red lK lV lLeft lRight ->
                    Node
                        Red
                        key
                        value
                        (Node Black lK lV lLeft lRight)
                        (Node Black rK lV rLeft rRight)

                _ ->
                    Node color rK rV (Node Red key value left rLeft) rRight

        _ ->
            case left of
                Node Red lK lV (Node Red llK llV llLeft llRight) lRight ->
                    Node
                        Red
                        lK
                        lV
                        (Node Black llK llV llLeft llRight)
                        (Node Black key value lRight right)

                _ ->
                    Node color key value left right


{-| Remove a key from a dict. If the key is not found,
no changes are made.
-}
remove : Sorter k -> k -> Dict k v -> Dict k v
remove sorter key dict =
    -- Root node is always Black
    case removeHelp (Sort.order sorter key) dict of
        Node Red k v l r ->
            Node Black k v l r

        x ->
            x


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : (k -> Order) -> Dict k v -> Dict k v
removeHelp sorter dict =
    case dict of
        Leaf ->
            Leaf

        Node color key value left right ->
            case sorter key of
                LT ->
                    case left of
                        Node Black _ _ lLeft _ ->
                            case lLeft of
                                Node Red _ _ _ _ ->
                                    Node color key value (removeHelp sorter left) right

                                _ ->
                                    case moveRedLeft dict of
                                        Node nColor nKey nValue nLeft nRight ->
                                            balance nColor nKey nValue (removeHelp sorter nLeft) nRight

                                        Leaf ->
                                            Leaf

                        _ ->
                            Node color key value (removeHelp sorter left) right

                _ ->
                    removeHelpEQGT sorter (removeHelpPrepEQGT dict color key value left right)


removeHelpPrepEQGT : Dict k v -> Color -> k -> v -> Dict k v -> Dict k v -> Dict k v
removeHelpPrepEQGT dict color key value left right =
    case left of
        Node Red lK lV lLeft lRight ->
            Node
                color
                lK
                lV
                lLeft
                (Node Red key value lRight right)

        _ ->
            case right of
                Node Black _ _ (Node Black _ _ _ _) _ ->
                    moveRedRight dict

                Node Black _ _ Leaf _ ->
                    moveRedRight dict

                _ ->
                    dict


{-| When we find the node we are looking for, we can remove by replacing the key-value
pair with the key-value pair of the left-most node on the right side (the closest pair).
-}
removeHelpEQGT : (k -> Order) -> Dict k v -> Dict k v
removeHelpEQGT sorter dict =
    case dict of
        Node color key value left right ->
            case sorter key of
                EQ ->
                    case getMin right of
                        Node _ minKey minValue _ _ ->
                            balance color minKey minValue left (removeMin right)

                        Leaf ->
                            Leaf

                _ ->
                    balance color key value left (removeHelp sorter right)

        Leaf ->
            Leaf


getMin : Dict k v -> Dict k v
getMin dict =
    case dict of
        Node _ _ _ ((Node _ _ _ _ _) as left) _ ->
            getMin left

        _ ->
            dict


removeMin : Dict k v -> Dict k v
removeMin dict =
    case dict of
        Node color key value ((Node lColor _ _ lLeft _) as left) right ->
            case lColor of
                Black ->
                    case lLeft of
                        Node Red _ _ _ _ ->
                            Node color key value (removeMin left) right

                        _ ->
                            case moveRedLeft dict of
                                Node nColor nKey nValue nLeft nRight ->
                                    balance nColor nKey nValue (removeMin nLeft) nRight

                                Leaf ->
                                    Leaf

                _ ->
                    Node color key value (removeMin left) right

        _ ->
            Leaf


moveRedLeft : Dict k v -> Dict k v
moveRedLeft dict =
    case dict of
        Node _ k v (Node _ lK lV lLeft lRight) (Node _ rK rV (Node Red rlK rlV rlL rlR) rRight) ->
            Node
                Red
                rlK
                rlV
                (Node Black k v (Node Red lK lV lLeft lRight) rlL)
                (Node Black rK rV rlR rRight)

        Node color k v (Node _ lK lV lLeft lRight) (Node _ rK rV rLeft rRight) ->
            case color of
                Black ->
                    Node
                        Black
                        k
                        v
                        (Node Red lK lV lLeft lRight)
                        (Node Red rK rV rLeft rRight)

                Red ->
                    Node
                        Black
                        k
                        v
                        (Node Red lK lV lLeft lRight)
                        (Node Red rK rV rLeft rRight)

        _ ->
            dict


moveRedRight : Dict k v -> Dict k v
moveRedRight dict =
    case dict of
        Node _ k v (Node _ lK lV (Node Red llK llV llLeft llRight) lRight) (Node _ rK rV rLeft rRight) ->
            Node Red lK lV (Node Black llK llV llLeft llRight) (Node Black k v lRight (Node Red rK rV rLeft rRight))

        Node color k v (Node _ lK lV lLeft lRight) (Node _ rK rV rLeft rRight) ->
            case color of
                Black ->
                    Node Black k v (Node Red lK lV lLeft lRight) (Node Red rK rV rLeft rRight)

                Red ->
                    Node Black k v (Node Red lK lV lLeft lRight) (Node Red rK rV rLeft rRight)

        _ ->
            dict


{-| Convert an association list into a dictionary.
-}
fromList : Sorter k -> List ( k, v ) -> Dict k v
fromList sorter assocs =
    List.foldl (\( key, value ) dict -> insert sorter key value dict) empty assocs


{-| Get the next highest key-value pair in the dictionary
-}
next : Sorter k -> k -> Dict k v -> Maybe ( k, v )
next sorter key dict =
    case dict of
        Node _ k v gt right ->
            nextHelper (Sort.order sorter key) Nothing k v gt right

        Leaf ->
            Nothing


nextHelper : (k -> Order) -> Maybe ( k, v ) -> k -> v -> Dict k v -> Dict k v -> Maybe ( k, v )
nextHelper sorter fallback key value gt right =
    case sorter key of
        EQ ->
            mink fallback right

        LT ->
            traverseNext sorter (Just ( key, value )) gt

        GT ->
            traverseNext sorter fallback right


traverseNext : (k -> Order) -> Maybe ( k, v ) -> Dict k v -> Maybe ( k, v )
traverseNext sorter fallback dict =
    case dict of
        Node _ key value gt right ->
            nextHelper sorter fallback key value gt right

        Leaf ->
            Nothing


mink : Maybe ( k, v ) -> Dict k v -> Maybe ( k, v )
mink pair dict =
    case dict of
        Node _ k v gt _ ->
            mink (Just ( k, v )) gt

        Leaf ->
            pair


{-| Get the next smallest key-value pair in the dictionary.
-}
previous : Sorter k -> k -> Dict k v -> Maybe ( k, v )
previous sorter key dict =
    case dict of
        Node _ k v gt right ->
            lastHelper (Sort.order sorter key) Nothing k v gt right

        Leaf ->
            Nothing


lastHelper : (k -> Order) -> Maybe ( k, v ) -> k -> v -> Dict k v -> Dict k v -> Maybe ( k, v )
lastHelper sorter fallback key value gt right =
    case sorter key of
        EQ ->
            maxk fallback gt

        LT ->
            traverseLast sorter fallback gt

        GT ->
            traverseLast sorter (Just ( key, value )) right


traverseLast : (k -> Order) -> Maybe ( k, v ) -> Dict k v -> Maybe ( k, v )
traverseLast sorter fallback dict =
    case dict of
        Node _ key value gt right ->
            lastHelper sorter fallback key value gt right

        Leaf ->
            Nothing


maxk : Maybe ( k, v ) -> Dict k v -> Maybe ( k, v )
maxk pair dict =
    case dict of
        Node _ k v _ right ->
            maxk (Just ( k, v )) right

        Leaf ->
            pair


{-| Reduce the key-value pairs in the dictionary from highest to lowest.
-}
foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
foldr func acc t =
    case t of
        Leaf ->
            acc

        Node _ key value left right ->
            foldr func (func key value (foldr func acc right)) left


{-| Reduce the key-value pairs in the dictionary from lowest to highest.
-}
foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
foldl func acc dict =
    case dict of
        Leaf ->
            acc

        Node _ key value left right ->
            foldl func (func key value (foldl func acc left)) right


{-| Get the smallest key-value pair in the dictionary.
-}
min : Dict k v -> Maybe ( k, v )
min dict =
    mink Nothing dict


{-| Get the highest key-value pair in the dictionary.
-}
max : Dict k v -> Maybe ( k, v )
max dict =
    maxk Nothing dict


{-| Create a dictionary with one entry.
-}
singleton : k -> v -> Dict k v
singleton key value =
    Node Black key value Leaf Leaf


{-| Get the number of entries in the dictionary.
-}
size : Dict k v -> Int
size dict =
    foldr (\_ _ n -> n + 1) 0 dict


{-| Apply a function to each value in the dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map func dict =
    case dict of
        Leaf ->
            Leaf

        Node color key value left right ->
            Node color key (func key value) (map func left) (map func right)


{-| Get a list of the keys in the dictionary.
-}
keys : Dict k v -> List k
keys dict =
    foldr (\k _ l -> k :: l) [] dict


{-| Get a list of the values in the dictionary.
-}
values : Dict k v -> List v
values dict =
    foldr (\_ v l -> v :: l) [] dict


{-| Convert a dictionary to an association list.
-}
toList : Dict k v -> List ( k, v )
toList dict =
    foldr (\k v l -> ( k, v ) :: l) [] dict


{-| Update the value of the given key.
-}
update : Sorter k -> k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update sorter key alter dict =
    case alter (get sorter key dict) of
        Just value ->
            insert sorter key value dict

        Nothing ->
            remove sorter key dict


{-| Keep key-value pairs that pass the filter.
-}
filter : Sorter k -> Filter ( k, v ) -> Dict k v -> Dict k v
filter sorter predicate dict =
    foldl (filterHelper sorter predicate) empty dict


filterHelper : Sorter k -> Filter ( k, v ) -> k -> v -> Dict k v -> Dict k v
filterHelper sorter predicate key value dict =
    case Filter.test predicate ( key, value ) of
        Filter.Pass ->
            insert sorter key value dict

        Filter.Fail ->
            dict
