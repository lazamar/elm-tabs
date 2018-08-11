module SelectedList
    exposing
        ( SelectedList
        , singleton
        , fromList
        , toList
        , select
        , selected
        )

import List.Extra


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


toList : SelectedList a -> List a
toList (SelectedList before selected after) =
    List.concat [ before, [ selected ], after ]


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
                (List.Extra.dropWhile ((/=) newSelected) list)


selected : SelectedList a -> a
selected (SelectedList _ s _) =
    s
