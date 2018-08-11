module View exposing (..)

import Types exposing (..)
import Html exposing (Html, div, text)


view : Model -> Html Msg
view model =
    div
        []
        [ text "I'm working!" ]
