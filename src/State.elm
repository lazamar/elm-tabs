module State exposing (init, update)

import Types exposing (..)
import Tabs


init : ( Model, Cmd Msg )
init =
    { tabs = Tabs.init TabsMsg
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            model ! []

        TabsMsg subMsg ->
            { model | tabs = Tabs.update subMsg model.tabs } ! []
