module Service.Set exposing (Set, max, foldl, foldr, fold, fromList, isEmpty, previous, get, member, root, next, remove, min, toList, map, insert, filter, singleton, size, empty)

{-| `Service.Set`s provide the exact same functionality as Elm's built-in `Set` type, while allowing for easy and safe extension with custom element types.

@docs Set, max, foldl, foldr, fold, fromList, isEmpty, previous, get, member, root, next, remove, min, toList, map, insert, filter, singleton, size, empty

    module MyRecordSet exposing (MyRecordSet, fromList, member {- , ... all the other implementations -})

    import Service.Set as Set exposing (Set)
    import Sort exposing (Sorter)

    type MyRecordSet
        = MyRecordSet (Set MyRecord)

    toSet : MyRecordSet -> Set MyRecord
    toSet (MyRecordSet set) =
        set

    type alias MyRecord =
        { foo : Int, bar : String }

    sorter : Sorter MyRecord
    sorter =
        Sort.by .foo Sort.int
            |> Sort.and (Sort.by .bar Sort.string)

    fromList : List MyRecord -> MyRecordSet
    fromList =
        Set.fromList sorter

    member : MyRecord -> Filter MyRecordSet
    member record =
        Filter.by toSet <| Set.member sorter record

    -- ... all the other implementations

-}

import Filter exposing (Filter)
import Fold exposing (Fold)
import Sort exposing (Sorter)


type Color
    = Red
    | Black


{-| A collection of unique values.
-}
type Set k
    = Node Color k (Set k) (Set k)
    | Leaf


{-| Get the root value of the set. Approximately the median.
-}
root : Set k -> Maybe k
root set =
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
idea: Sort.by Id.value Sort.int : Sorter Id
situation: have Int, not Id, but want to find Id
try: get (id -> compare (Id.value id) someNumber), could be generalized (Sorter a b := a -> b -> Order)
so, get : Sorter a k -> a -> Set k -> Maybe k

  - advantage of first approach is that you can use the same comparator the whole time
    or, get : (k -> Order) -> Set k -> Maybe k

set is only necessary collection, could just get rid of dict once we increase generality of member
more precisely than that, we want to say Sorter k = (b -> a) \* Sorter a = by derived Sort.derived

then we could do:
get : (b -> a) -> Sorter a -> Set b -> Maybe b

issue: what if we want to map the other key too? have to use sorter, since not necessarily maintaining eq (even though we know it is)

-}
member : Sorter k -> k -> Filter (Set k)
member sorter key =
    Filter.custom <| memberHelper (Sort.order sorter key)


memberHelper : (k -> Order) -> Set k -> Filter.Status
memberHelper sorter set =
    case set of
        Leaf ->
            Filter.Fail

        Node _ key lt gt ->
            case sorter key of
                EQ ->
                    Filter.Pass

                LT ->
                    memberHelper sorter lt

                GT ->
                    memberHelper sorter gt


{-| Get the value of a set member by its derived key.

    compareIdToInt : Int -> Id -> Order
    compareIdToInt val =
        Id.code >> Sort.order Sort.int val

    get (compareIdToInt 5) <| Sort.fromList [ Id 1, Id 2, Id 5]
    -- Id 5

-}
get : (k -> Order) -> Set k -> Maybe k
get sorter set =
    case set of
        Leaf ->
            Nothing

        Node _ key left right ->
            case sorter key of
                EQ ->
                    Just key

                LT ->
                    get sorter left

                GT ->
                    get sorter right


{-| Check if the set is empty.
-}
isEmpty : Filter (Set k)
isEmpty =
    Filter.custom <|
        \set ->
            case set of
                Leaf ->
                    Filter.Pass

                _ ->
                    Filter.Fail


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


{-| Insert an element in the set.
-}
fold : Sorter k -> Fold k (Set k)
fold sorter =
    Fold.custom <| fold_ sorter


fold_ : Sorter k -> Set k -> k -> Set k
fold_ sorter set value =
    insert sorter value set


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
fromList sorter =
    Fold.merge (Fold.listLeft <| fold sorter) empty


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


foldr_ : (b -> k -> b) -> b -> Set k -> b
foldr_ func acc t =
    case t of
        Leaf ->
            acc

        Node _ key left right ->
            foldr_ func (func (foldr_ func acc right) key) left


{-| Reduce the elements in the set from highest to lowest.
-}
foldr : Fold k b -> Fold (Set k) b
foldr folder =
    Fold.custom <| foldr_ <| Fold.merge folder


foldl_ : (b -> k -> b) -> b -> Set k -> b
foldl_ func acc dict =
    case dict of
        Leaf ->
            acc

        Node _ key left right ->
            foldl_ func (func (foldl_ func acc left) key) right


{-| Reduce the elements in the set from lowest to highest.
-}
foldl : Fold k b -> Fold (Set k) b
foldl folder =
    Fold.custom <| foldl_ <| Fold.merge folder


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
filter sorter predicate =
    Fold.merge (foldl <| Fold.passed predicate <| fold sorter) empty


{-| Convert a set to a list.
-}
toList : Set k -> List k
toList =
    Fold.merge (foldr Fold.list) []


{-| Construct a set with one element.
-}
singleton : k -> Set k
singleton key =
    Node Black key Leaf Leaf


{-| Get the number of elements in the set.
-}
size : Set k -> Int
size set =
    sizeHelper 0 set


sizeHelper : Int -> Set k -> Int
sizeHelper acc set =
    case set of
        Leaf ->
            acc

        Node _ _ left right ->
            sizeHelper (sizeHelper acc right) left


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
