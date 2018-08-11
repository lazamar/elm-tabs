module State exposing (init, update)

import Types exposing (..)


init : ( Model, Cmd Msg )
init =
    let
        doIrun val =
            Debug.log "Called at least once" "DoIrun"
    in
        Model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []
