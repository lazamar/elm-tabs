module Tabs exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import SelectedList exposing (SelectedList)
import List.Nonempty as Nonempty exposing (Nonempty)
import List.Extra


{-
   - Multiple divisions
   - Each division can have sub divisions

-}
-- ======================
-- TYPES
-- ======================


type Model msg
    = Model
        { toMsg : Msg -> msg
        , sections : Maybe (Section ID)
        }


type Msg msg
    = Close (Tab msg)


type alias ID =
    String


type alias Percentage =
    Int


type Orientation
    = Horizontal
    | Vertical


type Section a
    = TabGroup (SelectedList a)
    | Divider Orientation Percentage (Section a) (Section a)


type Tab msg
    = Tab (TabConfig msg) (Html msg)


type alias TabConfig msg =
    { id : ID
    , title : String
    , onClose : msg
    }



-- ======================
-- STATE
-- ======================


init : (Msg -> msg) -> Model msg
init toMsg =
    Model
        { toMsg = toMsg
        , sections = Nothing
        }


update : Msg -> Model msg -> Model msg
update msg (Model { toMsg, sections }) =
    Model
        { toMsg = toMsg
        , sections = sections
        }



-- ======================
-- VIEW
-- ======================


view : Model msg -> Nonempty (Tab msg) -> Html msg
view model tabs =
    div
        [ class "tabs" ]
        (tabs
            |> Nonempty.map renderTab
            |> Nonempty.toList
        )


tab : TabConfig msg -> (a -> Html msg) -> a -> Tab msg
tab config view content =
    Tab config (view content)


viewSection : (Msg -> msg) -> Section (Tab msg) -> Html msg
viewSection toMsg section =
    case section of
        TabGroup tabs ->
            div
                [ class "tabs-tabgroup" ]
                [ div [ class "tabs-tabgroup-headers" ]
                    (tabs
                        |> SelectedList.toTupleList
                        |> List.map tabHeader
                    )
                    |> Html.map toMsg
                ]

        Divider orientation percentage s1 s2 ->
            div
                [ classList
                    [ ( "tabs-divider", True )
                    , ( "tabs-divider--horizontal", orientation == Horizontal )
                    , ( "tabs-divider--vertical", orientation == Vertical )
                    ]
                ]
                CONTINUE
                FROM
                HERE
                []


tabHeader : ( Bool, Tab msg ) -> Html Msg
tabHeader ( selected, Tab { id, title } _ ) =
    div
        [ classList
            [ ( "tabs-tabgroup-header", True )
            , ( "tabs-tabgroup-header--selected", selected )
            ]
        ]
        [ text title ]


renderTab : Tab msg -> Html msg
renderTab (Tab config content) =
    div
        [ class "tab" ]
        [ div [ class "tab-title" ] [ text config.title ]
        , div [ class "tab-content" ] [ content ]
        ]


toLayout : Maybe (Section ID) -> Nonempty (Tab msg) -> Section (Tab msg)
toLayout mids tabs =
    let
        tabsList =
            Nonempty.toList tabs

        tabId (Tab { id } _) =
            id

        getTab id =
            List.Extra.find (tabId >> (==) id) tabsList

        msection =
            Maybe.andThen (fromSection getTab) mids
    in
        case msection of
            Nothing ->
                TabGroup (SelectedList.fromNonempty tabs)

            Just section ->
                section


fromSection : (ID -> Maybe (Tab msg)) -> Section ID -> Maybe (Section (Tab msg))
fromSection getTab section =
    case section of
        TabGroup idList ->
            case SelectedList.filterMap getTab idList of
                Nothing ->
                    Nothing

                Just tabs ->
                    Just <| TabGroup tabs

        Divider orientation percentage s1 s2 ->
            case fromSection getTab s1 of
                Nothing ->
                    fromSection getTab s2

                Just content1 ->
                    case fromSection getTab s2 of
                        Nothing ->
                            Just content1

                        Just content2 ->
                            Just <| Divider orientation percentage content1 content2
