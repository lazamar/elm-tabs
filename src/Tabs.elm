module Tabs exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type alias Title =
    String


type alias Content msg =
    Html msg


type Tab msg
    = Tab Title (() -> Html msg)


tab : Title -> (a -> Html msg) -> a -> Tab msg
tab title view content =
    Tab title (\() -> view content)


tabs : List (Tab msg) -> Html msg
tabs tabList =
    div
        [ class "tabs" ]
        (List.map renderTab tabList)


renderTab : Tab msg -> Html msg
renderTab (Tab title toContent) =
    div
        [ class "tab" ]
        [ div [ class "tab-title" ] [ text title ]
        , div [ class "tab-content" ] [ toContent () ]
        ]



--tabs : (List Tab msg
--tabs
--    [ Tab Title Content
--    ]
