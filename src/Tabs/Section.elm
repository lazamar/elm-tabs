module Tabs.Section exposing (..)

import Tabs.SelectList as SelectList exposing (SelectList)
import Tabs.Tree as Tree exposing (Tree(Node, Leaf))
import List.Nonempty as Nonempty exposing (Nonempty)


{-|

    Tab Sections are a tree structure

           Divider 1
             /  \
      TabGroup  Divider 2
                   /   \
            TabGroup  TabGroup
-}
type alias Section a =
    Tree DividerInfo (SelectList a)


type Orientation
    = Horizontal
    | Vertical


type alias DividerInfo =
    { orientation : Orientation

    -- Distance in pixels between the middle of
    -- the divider element and the divider's division line
    , offset : Int
    }



-- CONSTRUCTORS


divider : DividerInfo -> Section a -> Section a -> Section a
divider info s1 s2 =
    Node info s1 s2


tabsGroup : Nonempty a -> Section a
tabsGroup nonempty =
    Leaf <| SelectList.fromNonempty nonempty



-- UTILS


toList : Section a -> List a
toList s =
    s
        |> Tree.leaves
        |> List.map SelectList.toList
        |> List.concat


map : (a -> b) -> Section a -> Section b
map f s =
    Tree.map (SelectList.map f) s


filterMap : (a -> Maybe b) -> Section a -> Maybe (Section b)
filterMap f s =
    Tree.filterMap (SelectList.filterMap f) s


select : a -> Section a -> Section a
select elem s =
    Tree.map (SelectList.select elem) s


toHtml : (Int -> DividerInfo -> c -> c -> c) -> (SelectList b -> c) -> Section b -> c
toHtml fnode fleaf tree =
    tree
        |> Tree.indexedMapNodes (,)
        |> toHtml_ fnode fleaf


toHtml_ :
    (Int -> DividerInfo -> c -> c -> c)
    -> (SelectList b -> c)
    -> Tree ( Int, DividerInfo ) (SelectList b)
    -> c
toHtml_ fnode fleaf tree =
    case tree of
        Leaf a ->
            fleaf a

        Node ( idx, a ) t1 t2 ->
            fnode
                idx
                a
                (toHtml_ fnode fleaf t1)
                (toHtml_ fnode fleaf t2)
