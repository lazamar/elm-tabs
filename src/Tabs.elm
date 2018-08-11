module Tabs exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import SelectedList exposing (SelectedList)
import List.Nonempty as Nonempty exposing (Nonempty)


{-
   - Multiple divisions
   - Each division can have sub divisions

-}
-- ======================
-- TYPES
-- ======================


type Model msg
    = Model
        { toMsg : Msg -> msg
        , sections : Maybe (Section ID)
        }


type Msg msg
    = Close (Tab msg)


type alias ID =
    String


type alias Percentage =
    Int


type Orientation
    = Horizontal
    | Vertical


type Section a
    = TabGroup (SelectedList a)
    | Divider Orientation Percentage (Section a) (Section a)


type Tab msg
    = Tab (TabConfig msg) (Html msg)


type alias TabConfig msg =
    { id : ID
    , title : String
    , onClose : msg
    }



-- ======================
-- STATE
-- ======================


init : (Msg -> msg) -> Model msg
init toMsg =
    Model
        { toMsg = toMsg
        , sections = Nothing
        }


update : Msg -> Model msg -> Model msg
update msg (Model { toMsg, sections }) =
    Model
        { toMsg = toMsg
        , sections = sections
        }



-- ======================
-- VIEW
-- ======================


view : Model msg -> Nonempty (Tab msg) -> Html msg
view model tabs =
    div
        [ class "tabs" ]
        (tabs
            |> Nonempty.map renderTab
            |> Nonempty.toList
        )


tab : TabConfig msg -> (a -> Html msg) -> a -> Tab msg
tab config view content =
    Tab config (view content)


renderTab : Tab msg -> Html msg
renderTab (Tab config content) =
    div
        [ class "tab" ]
        [ div [ class "tab-title" ] [ text config.title ]
        , div [ class "tab-content" ] [ content ]
        ]
