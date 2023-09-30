module Service.Set exposing (Set, max, foldl, foldr, fromList, isEmpty, previous, member, middle, next, remove, min, toList, map, insert, filter, singleton, size)

{-| `Service.Set`s provide the exact same functionality as Elm's built-in `Set` type, while allowing for easy and safe extension with custom element types.

@docs Set, max, foldl, foldr, fromList, isEmpty, previous, member, middle, next, remove, min, toList, map, insert, filter, singleton, size

    module MyRecordSet exposing (MyRecordSet, fromList, member {- , ... all the other implementations -})

    import Service.Set as Set exposing (Set)
    import Sort exposing (Sorter)

    type MyRecordSet
        = MyRecordSet (Set MyRecord)

    type alias MyRecord =
        { foo : Int, bar : String }

    sorter : Sorter MyRecord
    sorter =
        Sort.by .foo Sort.int
            |> Sort.and (Sort.by .bar Sort.string)

    fromList : List MyRecord -> MyRecordSet
    fromList =
        Set.fromList sorter

    member : MyRecord -> MyRecordSet -> Bool
    member record (MyRecordSet set) =
        Set.member sorter record set

    -- ... all the other implementations

-}

import Filter exposing (Filter)
import Sort exposing (Sorter)


type Color
    = Red
    | Black


{-| A collection of unique values.
-}
type Set k
    = Node Color k (Set k) (Set k)
    | Leaf


{-| Get the middle value of the set.
-}
middle : Set k -> Maybe k
middle set =
    case set of
        Node _ key _ _ ->
            Just key

        Leaf ->
            Nothing


{-| An empty set.
-}
empty : Set k
empty =
    Leaf


{-| Check if a value is in the set.
-}
member : Sorter k -> k -> Set k -> Bool
member sorter key set =
    memberHelper (Sort.order sorter key) set


memberHelper : (k -> Order) -> Set k -> Bool
memberHelper sorter set =
    case set of
        Leaf ->
            False

        Node _ key lt gt ->
            case sorter key of
                EQ ->
                    True

                LT ->
                    memberHelper sorter lt

                GT ->
                    memberHelper sorter gt


{-| Check if the set is empty.
-}
isEmpty : Set k -> Bool
isEmpty set =
    case set of
        Leaf ->
            True

        _ ->
            False


{-| Insert an element in the set.
-}
insert : Sorter k -> k -> Set k -> Set k
insert sorter key dict =
    -- Root node is always Black
    case insertHelp sorter key dict of
        Node Red k l r ->
            Node Black k l r

        x ->
            x


insertHelp : Sorter k -> k -> Set k -> Set k
insertHelp sorter key dict =
    case dict of
        Leaf ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            Node Red key Leaf Leaf

        Node nColor nKey nLeft nRight ->
            case Sort.order sorter key nKey of
                LT ->
                    balance nColor nKey (insertHelp sorter key nLeft) nRight

                EQ ->
                    Node nColor nKey nLeft nRight

                GT ->
                    balance nColor nKey nLeft (insertHelp sorter key nRight)


balance : Color -> k -> Set k -> Set k -> Set k
balance color key left right =
    case right of
        Node Red rK rLeft rRight ->
            case left of
                Node Red lK lLeft lRight ->
                    Node
                        Red
                        key
                        (Node Black lK lLeft lRight)
                        (Node Black rK rLeft rRight)

                _ ->
                    Node color rK (Node Red key left rLeft) rRight

        _ ->
            case left of
                Node Red lK (Node Red llK llLeft llRight) lRight ->
                    Node
                        Red
                        lK
                        (Node Black llK llLeft llRight)
                        (Node Black key lRight right)

                _ ->
                    Node color key left right


{-| Remove a key from a set. If the key is not found,
no changes are made.
-}
remove : Sorter k -> k -> Set k -> Set k
remove sorter key dict =
    -- Root node is always Black
    case removeHelp (Sort.order sorter key) dict of
        Node Red k l r ->
            Node Black k l r

        x ->
            x


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : (k -> Order) -> Set k -> Set k
removeHelp sorter set =
    case set of
        Leaf ->
            Leaf

        Node color key left right ->
            if sorter key == LT then
                case left of
                    Node Black _ lLeft _ ->
                        case lLeft of
                            Node Red _ _ _ ->
                                Node color key (removeHelp sorter left) right

                            _ ->
                                case moveRedLeft set of
                                    Node nColor nKey nLeft nRight ->
                                        balance nColor nKey (removeHelp sorter nLeft) nRight

                                    Leaf ->
                                        Leaf

                    _ ->
                        Node color key (removeHelp sorter left) right

            else
                removeHelpEQGT sorter (removeHelpPrepEQGT set color key left right)


removeHelpPrepEQGT : Set k -> Color -> k -> Set k -> Set k -> Set k
removeHelpPrepEQGT set color key left right =
    case left of
        Node Red lK lLeft lRight ->
            Node
                color
                lK
                lLeft
                (Node Red key lRight right)

        _ ->
            case right of
                Node Black _ (Node Black _ _ _) _ ->
                    moveRedRight set

                Node Black _ Leaf _ ->
                    moveRedRight set

                _ ->
                    set


{-| When we find the node we are looking for, we can remove by replacing the key-value
pair with the key-value pair of the left-most node on the right side (the closest pair).
-}
removeHelpEQGT : (k -> Order) -> Set k -> Set k
removeHelpEQGT sorter set =
    case set of
        Node color key left right ->
            if sorter key == EQ then
                case getMin right of
                    Node _ minKey _ _ ->
                        balance color minKey left (removeMin right)

                    Leaf ->
                        Leaf

            else
                balance color key left (removeHelp sorter right)

        Leaf ->
            Leaf


getMin : Set k -> Set k
getMin set =
    case set of
        Node _ _ ((Node _ _ _ _) as left) _ ->
            getMin left

        _ ->
            set


removeMin : Set k -> Set k
removeMin set =
    case set of
        Node color key ((Node lColor _ lLeft _) as left) right ->
            case lColor of
                Black ->
                    case lLeft of
                        Node Red _ _ _ ->
                            Node color key (removeMin left) right

                        _ ->
                            case moveRedLeft set of
                                Node nColor nKey nLeft nRight ->
                                    balance nColor nKey (removeMin nLeft) nRight

                                Leaf ->
                                    Leaf

                _ ->
                    Node color key (removeMin left) right

        _ ->
            Leaf


moveRedLeft : Set k -> Set k
moveRedLeft set =
    case set of
        Node _ k (Node _ lK lLeft lRight) (Node _ rK (Node Red rlK rlL rlR) rRight) ->
            Node
                Red
                rlK
                (Node Black k (Node Red lK lLeft lRight) rlL)
                (Node Black rK rlR rRight)

        Node clr k (Node _ lK lLeft lRight) (Node _ rK rLeft rRight) ->
            case clr of
                Black ->
                    Node
                        Black
                        k
                        (Node Red lK lLeft lRight)
                        (Node Red rK rLeft rRight)

                Red ->
                    Node
                        Black
                        k
                        (Node Red lK lLeft lRight)
                        (Node Red rK rLeft rRight)

        _ ->
            set


moveRedRight : Set k -> Set k
moveRedRight set =
    case set of
        Node _ k (Node _ lK (Node Red llK llLeft llRight) lRight) (Node _ rK rLeft rRight) ->
            Node Red lK (Node Black llK llLeft llRight) (Node Black k lRight (Node Red rK rLeft rRight))

        Node clr k (Node _ lK lLeft lRight) (Node _ rK rLeft rRight) ->
            case clr of
                Black ->
                    Node Black k (Node Red lK lLeft lRight) (Node Red rK rLeft rRight)

                Red ->
                    Node Black k (Node Red lK lLeft lRight) (Node Red rK rLeft rRight)

        _ ->
            set


{-| Convert a list into a set.
-}
fromList : Sorter k -> List k -> Set k
fromList sorter assocs =
    List.foldl (\key set -> insert sorter key set) empty assocs


{-| Get the next highest element in the set.
-}
next : Sorter k -> k -> Set k -> Maybe k
next sorter val set =
    case set of
        Node _ k lt gt ->
            nextHelper (Sort.order sorter val) Nothing k lt gt

        Leaf ->
            Nothing


nextHelper : (k -> Order) -> Maybe k -> k -> Set k -> Set k -> Maybe k
nextHelper sorter fallback key lt gt =
    case sorter key of
        EQ ->
            mink fallback gt

        LT ->
            traverseNext sorter (Just key) lt

        GT ->
            traverseNext sorter fallback gt


traverseNext : (k -> Order) -> Maybe k -> Set k -> Maybe k
traverseNext sorter fallback set =
    case set of
        Node _ key lt gt ->
            nextHelper sorter fallback key lt gt

        Leaf ->
            Nothing


mink : Maybe k -> Set k -> Maybe k
mink key set =
    case set of
        Node _ k lt _ ->
            mink (Just k) lt

        Leaf ->
            key


{-| Get the next smallest element in the set.
-}
previous : Sorter k -> k -> Set k -> Maybe k
previous sorter val set =
    case set of
        Node _ k lt gt ->
            lastHelper (Sort.order sorter val) Nothing k lt gt

        Leaf ->
            Nothing


lastHelper : (k -> Order) -> Maybe k -> k -> Set k -> Set k -> Maybe k
lastHelper sorter fallback key lt gt =
    case sorter key of
        EQ ->
            maxk fallback lt

        LT ->
            traverseLast sorter fallback lt

        GT ->
            traverseLast sorter (Just key) gt


traverseLast : (k -> Order) -> Maybe k -> Set k -> Maybe k
traverseLast sorter fallback set =
    case set of
        Node _ key lt gt ->
            lastHelper sorter fallback key lt gt

        Leaf ->
            Nothing


maxk : Maybe k -> Set k -> Maybe k
maxk key set =
    case set of
        Node _ k _ gt ->
            maxk (Just k) gt

        Leaf ->
            key


{-| Reduce the elements in the set from highest to lowest.
-}
foldr : (k -> b -> b) -> b -> Set k -> b
foldr func acc t =
    case t of
        Leaf ->
            acc

        Node _ key left right ->
            foldr func (func key (foldr func acc right)) left


{-| Reduce the elements in the set from lowest to highest.
-}
foldl : (k -> b -> b) -> b -> Set k -> b
foldl func acc dict =
    case dict of
        Leaf ->
            acc

        Node _ key left right ->
            foldl func (func key (foldl func acc left)) right


{-| Apply a function to each element in the set.
-}
map : (a -> b) -> Set a -> Set b
map func set =
    case set of
        Leaf ->
            Leaf

        Node color key left right ->
            Node color (func key) (map func left) (map func right)


{-| Keep items that pass the filter.
-}
filter : Sorter k -> Filter k -> Set k -> Set k
filter sorter predicate set =
    foldl (filterHelper sorter predicate) empty set


filterHelper : Sorter k -> Filter k -> k -> Set k -> Set k
filterHelper sorter predicate key set =
    case Filter.test predicate key of
        Filter.Pass ->
            insert sorter key set

        Filter.Fail ->
            set


{-| Convert a set to a list.
-}
toList : Set k -> List k
toList set =
    foldr (::) [] set


{-| Construct a set with one element.
-}
singleton : k -> Set k
singleton key =
    Node Black key Leaf Leaf


{-| Get the number of elements in the set.
-}
size : Set k -> Int
size set =
    foldr (\_ n -> n + 1) 0 set


{-| Get the smallest element in the set.
-}
min : Set k -> Maybe k
min set =
    mink Nothing set


{-| Get the highest element in the set.
-}
max : Set k -> Maybe k
max set =
    maxk Nothing set