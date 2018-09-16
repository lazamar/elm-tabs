module Tabs exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import Tabs.SelectList as SelectList exposing (SelectList)
import List.Nonempty as Nonempty exposing (Nonempty)
import List.Extra
import Tabs.Section as Section
    exposing
        ( Section
        , Orientation(Vertical, Horizontal)
        , DividerInfo
        )
import Tabs.Tree as Tree exposing (Tree(Node, Leaf))


{-
   - Multiple divisions
   - Each division can have sub divisions

-}
-- ======================
-- TYPES
-- ======================


type Model msg
    = Model
        { toMsg : Msg msg -> msg
        , sections : Maybe (Section TabId)
        }


type Msg msg
    = SelectTab (Root msg) TabId


{-| Represents the root of the section tree
-}
type Root a
    = Root (Section (Tab a))


type alias Tab msg =
    { id : TabId
    , title : String
    , onClose : msg
    , content : Html msg
    }


type alias TabId =
    String



-- ======================
-- STATE
-- ======================


init : (Msg msg -> msg) -> Model msg
init toMsg =
    Model
        { toMsg = toMsg
        , sections =
            Just <|
                Node
                    { orientation = Vertical
                    , offset = 40
                    }
                    (Leaf <| SelectList.singleton "1")
                    (Leaf <| SelectList.singleton "2")
        }


update : Msg msg -> Model msg -> Model msg
update msg model =
    case msg of
        SelectTab root id ->
            model
                |> updateSections root
                |> selectTab id


updateSections : Root msg -> Model msg -> Model msg
updateSections (Root section) (Model model) =
    Model { model | sections = Just <| Section.map .id section }


selectTab : TabId -> Model msg -> Model msg
selectTab id (Model model) =
    Model { model | sections = model.sections |> Maybe.map (Section.select id) }



-- ======================
-- VIEW
-- ======================


view : Model msg -> Nonempty (Tab msg) -> Html msg
view (Model model) tabs =
    tabs
        |> toSections model.sections
        |> (\s -> viewSection model.toMsg (Root s) s)


viewSection : (Msg msg -> msg) -> Root msg -> Section (Tab msg) -> Html msg
viewSection toMsg root section =
    Section.toHtml
        (viewDivider toMsg root)
        (viewTabGroup toMsg root)
        section


viewTabGroup : (Msg msg -> msg) -> Root msg -> SelectList (Tab msg) -> Html msg
viewTabGroup toMsg root tabs =
    div
        [ class "tabs-tabgroup" ]
        [ tabs
            |> SelectList.toTupleList
            |> List.map (tabHeader root)
            |> div [ class "tabs-tabgroup-headers" ]
            |> Html.map toMsg
        , tabs
            |> SelectList.selected
            |> viewTab
        ]


viewDivider : (Msg msg -> msg) -> Root msg -> Int -> DividerInfo -> Html msg -> Html msg -> Html msg
viewDivider toMsg root idx { orientation, offset } s1 s2 =
    div
        [ classList
            [ ( "tabs-divider", True )
            , ( "tabs-divider--horizontal", orientation == Horizontal )
            , ( "tabs-divider--vertical", orientation == Vertical )
            ]
        ]
        [ division orientation offset s1
        , div [ class "tabs-divider-line" ] []
        , division orientation (-offset) s2
        ]


division : Orientation -> Int -> Html msg -> Html msg
division orientation offset content =
    div
        [ class "tabs-divider-division"
        , style
            [ ( case orientation of
                    Horizontal ->
                        "width"

                    Vertical ->
                        "height"
              , "calc(50% + " ++ toString offset ++ "px)"
              )
            ]
        ]
        [ content ]


tabHeader : Root msg -> ( Bool, Tab msg ) -> Html (Msg msg)
tabHeader root ( selected, { id, title } ) =
    div
        [ classList
            [ ( "tabs-tabgroup-header", True )
            , ( "tabs-tabgroup-header--selected", selected )
            ]
        , onClick (SelectTab root id)
        ]
        [ text title ]


viewTab : Tab msg -> Html msg
viewTab tab =
    div
        [ class "tabs-tabgroup-tab" ]
        [ tab.content ]


{-| We have a tree structure specifying how the tabs should be laid
out int the view, but we don't know if all the tabs for which we
have ids, are still being shown. Given a tree structure with IDS
and some tabs, it will return a new tree structure with all tabs
that match the IDs passed.

If there are new tabs (tabs that are not in the structure with ids)
these will be inserted as a new section at the root

-}
toSections : Maybe (Section TabId) -> Nonempty (Tab msg) -> Section (Tab msg)
toSections mids tabs =
    let
        -- tabs whose layout is specified in mids
        mordered =
            Maybe.andThen (Section.filterMap getTab) mids

        getTab id =
            List.Extra.find (.id >> (==) id) tabsList

        tabsList =
            Nonempty.toList tabs

        -- New tabs. Those that are not in our organised ids
        munordered =
            tabs
                |> Nonempty.toList
                |> List.filter (not << flip List.member orderedAsList)
                |> Nonempty.fromList
                |> Maybe.map Section.tabsGroup

        orderedAsList =
            mordered
                |> Maybe.map Section.toList
                |> Maybe.withDefault []
    in
        case mordered of
            Nothing ->
                -- All tabs are new
                Section.tabsGroup tabs

            Just ordered ->
                case munordered of
                    Nothing ->
                        -- No new tabs, everything was already laid out in sections
                        ordered

                    Just unordered ->
                        -- Some tabs were already laid out and
                        -- there are new ones as well
                        Section.divider
                            { orientation = Horizontal, offset = 0 }
                            unordered
                            ordered
