module Main exposing (..)

import View exposing (view)
import Types exposing (Model, Msg)
import State exposing (init, update)
import Html


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
