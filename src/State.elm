module State exposing (init, update)

import Types exposing (..)


init : ( Model, Cmd Msg )
init =
    Debug.log "Called at least once" Model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []
