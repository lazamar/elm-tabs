module SelectedList
    exposing
        ( SelectedList
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


type SelectedList a
    = SelectedList (List a) a (List a)


singleton : a -> SelectedList a
singleton v =
    SelectedList [] v []


fromList : List a -> Maybe (SelectedList a)
fromList l =
    case List.head l of
        Nothing ->
            Nothing

        Just a ->
            Just <| SelectedList [] a (List.drop 1 l)


fromNonempty : Nonempty a -> SelectedList a
fromNonempty nlist =
    SelectedList [] (Nonempty.head nlist) (Nonempty.tail nlist)


toList : SelectedList a -> List a
toList (SelectedList before selected after) =
    List.concat [ before, [ selected ], after ]


toTupleList : SelectedList a -> List ( Bool, a )
toTupleList (SelectedList before selected after) =
    List.concat
        [ List.map (\a -> ( False, a )) before
        , [ ( True, selected ) ]
        , List.map (\a -> ( False, a )) after
        ]


select : SelectedList a -> a -> SelectedList a
select slist newSelected =
    let
        list =
            toList slist
    in
        if not <| List.member newSelected list then
            slist
        else
            SelectedList
                (List.Extra.takeWhile ((/=) newSelected) list)
                newSelected
                (list
                    |> List.Extra.dropWhile ((/=) newSelected)
                    |> List.drop 1
                )


selected : SelectedList a -> a
selected (SelectedList _ s _) =
    s


{-| if the selected element is filtered out, the first element
becomes selected.

If all elements are filtered out, the result is Nothing

-}
filterMap : (a -> Maybe b) -> SelectedList a -> Maybe (SelectedList b)
filterMap f (SelectedList before selected after) =
    case f selected of
        Just v ->
            Just <|
                SelectedList
                    (List.filterMap f before)
                    v
                    (List.filterMap f after)

        Nothing ->
            [ before, after ]
                |> List.concat
                |> List.filterMap f
                |> fromList


filter : (a -> Bool) -> SelectedList a -> Maybe (SelectedList a)
filter f (SelectedList before selected after) =
    if f selected then
        Just <|
            SelectedList
                (List.filter f before)
                selected
                (List.filter f after)
    else
        [ before, after ]
            |> List.concat
            |> List.filter f
            |> fromList


map : (a -> b) -> SelectedList a -> SelectedList b
map f (SelectedList before selected after) =
    SelectedList
        (List.map f before)
        (f selected)
        (List.map f after)
