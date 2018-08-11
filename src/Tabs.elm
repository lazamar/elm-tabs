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


type alias Title =
    String


type alias Content msg =
    Html msg


type Tab msg
    = Tab Title msg (() -> Html msg)


tab : Title -> msg -> (a -> Html msg) -> a -> Tab msg
tab title onClose view content =
    Tab title onClose (\() -> view content)


tabs : List (Tab msg) -> Html msg
tabs tabList =
    div
        [ class "tabs" ]
        (List.map renderTab tabList)


renderTab : Tab msg -> Html msg
renderTab (Tab title onClose toContent) =
    div
        [ class "tab" ]
        [ div [ class "tab-title" ] [ text title ]
        , div [ class "tab-content" ] [ toContent () ]
        ]
