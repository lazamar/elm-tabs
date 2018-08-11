module View exposing (..)

import Types exposing (..)
import Html exposing (Html, div, text, h3)
import Tabs
import List.Nonempty as Nonempty


view : Model -> Html Msg
view model =
    let
        mtabs =
            Nonempty.fromList
                [ Tabs.tab
                    { id = "1"
                    , title = "First Tab"
                    , onClose = DoNothing
                    }
                    showFirst
                    "Here is some content"
                , Tabs.tab
                    { id = "2"
                    , title = "Second tab"
                    , onClose = DoNothing
                    }
                    showSecond
                    2
                ]
    in
        div
            []
            [ text "I'm working!"
            , case mtabs of
                Nothing ->
                    text "No tabs to show"

                Just tabs ->
                    Tabs.view model.tabs tabs
            ]


showFirst : String -> Html Msg
showFirst someText =
    h3 [] [ text someText ]


showSecond : Int -> Html Msg
showSecond number =
    h3 [] [ text <| "The number is " ++ toString number ]
