module Tabs exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import SelectedList exposing (SelectedList)


{-
   - Multiple divisions
   - Each division can have sub divisions

-}


type alias Model msg =
    { toMsg : Msg -> msg
    }


type Msg msg
    = Close (Tab msg)


type alias Percentage =
    Int


type Orientation
    = Horizontal
    | Vertical


type Section msg
    = TabGroup (SelectedList (Tab msg))
    | Divider Orientation Percentage Section Section


type Tab msg
    = Tab (TabConfig msg) (Html msg)


type alias TabConfig msg =
    { id : String
    , title : String
    , onClose : msg
    }


tab : TabConfig msg -> (a -> Html msg) -> a -> Tab msg
tab config view content =
    Tab config (view content)


tabs : List (Tab msg) -> Html msg
tabs tabList =
    div
        [ class "tabs" ]
        (List.map renderTab tabList)


renderTab : Tab msg -> Html msg
renderTab (Tab config content) =
    div
        [ class "tab" ]
        [ div [ class "tab-title" ] [ text config.title ]
        , div [ class "tab-content" ] [ content ]
        ]
