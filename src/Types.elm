module Types exposing (..)

import Tabs


type alias Model =
    { tabs : Tabs.Model Msg
    }


type Msg
    = DoNothing
    | TabsMsg Tabs.Msg
