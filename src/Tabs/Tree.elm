module Tabs.Tree exposing (..)


type Tree a b
    = Node a (Tree a b) (Tree a b)
    | Leaf b


map : (b -> c) -> Tree a b -> Tree a c
map f t =
    case t of
        Leaf a ->
            Leaf (f a)

        Node a t1 t2 ->
            Node a (map f t1) (map f t2)


filterMap : (b -> Maybe c) -> Tree a b -> Maybe (Tree a c)
filterMap f t =
    case t of
        Leaf a ->
            Maybe.map Leaf (f a)

        Node a t1 t2 ->
            case filterMap f t1 of
                Nothing ->
                    filterMap f t2

                Just newT1 ->
                    case filterMap f t2 of
                        Nothing ->
                            Just newT1

                        Just newT2 ->
                            Just (Node a newT1 newT2)


mapNodes : (a -> c) -> Tree a b -> Tree c b
mapNodes f t =
    case t of
        Leaf a ->
            Leaf a

        Node a t1 t2 ->
            Node (f a) (mapNodes f t1) (mapNodes f t2)


indexedMapNodes : (Int -> a -> c) -> Tree a b -> Tree c b
indexedMapNodes f t =
    indexedMapNodes_ f t 0
        |> Tuple.second


indexedMapNodes_ : (Int -> a -> c) -> Tree a b -> Int -> ( Int, Tree c b )
indexedMapNodes_ f t start =
    case t of
        Leaf a ->
            ( start, Leaf a )

        Node a t1 t2 ->
            let
                ( nextStart1, newT1 ) =
                    indexedMapNodes_ f t1 start

                ( nextStart2, newT2 ) =
                    indexedMapNodes_ f t2 nextStart1
            in
                ( nextStart2 + 1, Node (f nextStart2 a) newT1 newT2 )


updateNodeAtIndex : Int -> (a -> a) -> Tree a b -> Tree a b
updateNodeAtIndex targetIdx f t =
    indexedMapNodes
        (\idx a ->
            if idx == targetIdx then
                f a
            else
                a
        )
        t


leaves : Tree a b -> List b
leaves t =
    case t of
        Leaf a ->
            [ a ]

        Node a t1 t2 ->
            List.concat [ leaves t1, leaves t2 ]
