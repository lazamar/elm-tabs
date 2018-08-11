module View exposing (..)

import Types exposing (..)
import Html exposing (Html, div, text, h3)
import Tabs


view : Model -> Html Msg
view model =
    div
        []
        [ text "I'm working!"
        , Tabs.tabs
            [ Tabs.tab "First Tab" showFirst "Here is some content"
            , Tabs.tab "Second Tab" showSecond 2
            ]
        ]


showFirst : String -> Html Msg
showFirst someText =
    h3 [] [ text someText ]


showSecond : Int -> Html Msg
showSecond number =
    h3 [] [ text <| "The number is " ++ toString number ]
