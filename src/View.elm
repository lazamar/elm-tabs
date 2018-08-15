module View exposing (..)

import Types exposing (..)
import Html exposing (Html, div, text, h3)
import Html.Attributes exposing (style)
import Tabs
import List.Nonempty as Nonempty


view : Model -> Html Msg
view model =
    let
        mtabs =
            Nonempty.fromList
                [ { id = "1"
                  , title = "First Tab"
                  , onClose = DoNothing
                  , content = showFirst "Here is some content"
                  }
                , { id = "2"
                  , title = "Second tab"
                  , onClose = DoNothing
                  , content = showSecond 2
                  }
                , { id = "3"
                  , title = "Third tab"
                  , onClose = DoNothing
                  , content = showThird True
                  }
                ]
    in
        div
            [ style [ ( "height", "100%" ) ] ]
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


showThird : Bool -> Html Msg
showThird v =
    text <| "THIRD TAB: The value is " ++ toString v
