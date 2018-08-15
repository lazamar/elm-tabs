module Tabs.SelectList
    exposing
        ( SelectList
        , singleton
        , fromList
        , fromNonempty
        , toList
        , toTupleList
        , select
        , selected
        , map
        , filter
        , filterMap
        )

import List.Extra
import List.Nonempty as Nonempty exposing (Nonempty)


{-
   This is a non-empty list that always has
   exactly one selected element

   This is an example of a ZipList
-}


type SelectList a
    = SelectList (List a) a (List a)


singleton : a -> SelectList a
singleton v =
    SelectList [] v []


fromList : List a -> Maybe (SelectList a)
fromList l =
    case List.head l of
        Nothing ->
            Nothing

        Just a ->
            Just <| SelectList [] a (List.drop 1 l)


fromNonempty : Nonempty a -> SelectList a
fromNonempty nlist =
    SelectList [] (Nonempty.head nlist) (Nonempty.tail nlist)


toList : SelectList a -> List a
toList (SelectList before selected after) =
    List.concat [ before, [ selected ], after ]


toTupleList : SelectList a -> List ( Bool, a )
toTupleList (SelectList before selected after) =
    List.concat
        [ List.map (\a -> ( False, a )) before
        , [ ( True, selected ) ]
        , List.map (\a -> ( False, a )) after
        ]


select : a -> SelectList a -> SelectList a
select newSelected slist =
    let
        list =
            toList slist
    in
        if not <| List.member newSelected list then
            slist
        else
            SelectList
                (List.Extra.takeWhile ((/=) newSelected) list)
                newSelected
                (list
                    |> List.Extra.dropWhile ((/=) newSelected)
                    |> List.drop 1
                )


selected : SelectList a -> a
selected (SelectList _ s _) =
    s


{-| if the selected element is filtered out, the first element
becomes selected.

If all elements are filtered out, the result is Nothing

-}
filterMap : (a -> Maybe b) -> SelectList a -> Maybe (SelectList b)
filterMap f (SelectList before selected after) =
    case f selected of
        Just v ->
            Just <|
                SelectList
                    (List.filterMap f before)
                    v
                    (List.filterMap f after)

        Nothing ->
            [ before, after ]
                |> List.concat
                |> List.filterMap f
                |> fromList


filter : (a -> Bool) -> SelectList a -> Maybe (SelectList a)
filter f (SelectList before selected after) =
    if f selected then
        Just <|
            SelectList
                (List.filter f before)
                selected
                (List.filter f after)
    else
        [ before, after ]
            |> List.concat
            |> List.filter f
            |> fromList


map : (a -> b) -> SelectList a -> SelectList b
map f (SelectList before selected after) =
    SelectList
        (List.map f before)
        (f selected)
        (List.map f after)
