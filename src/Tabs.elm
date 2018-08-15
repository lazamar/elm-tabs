module Tabs exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import SelectedList exposing (SelectedList)
import List.Nonempty as Nonempty exposing (Nonempty)
import List.Extra


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

        -- Each section divider has an TabId.
        , lastDividerId : DividerId
        }


type Msg msg
    = SelectTab (Root msg) TabId



-- TAB


type alias TabId =
    String


type Tab msg
    = Tab (TabConfig msg) (Html msg)


type alias TabConfig msg =
    { id : TabId
    , title : String
    , onClose : msg
    }



-- SECTION


type Section a
    = TabGroup (SelectedList a)
    | Divider DividerInfo (Section a) (Section a)


type alias DividerId =
    Int


type Orientation
    = Horizontal
    | Vertical


type alias DividerInfo =
    { id : DividerId
    , orientation : Orientation

    -- Distance in pixels between the middle of
    -- the divider element and the divider's division line
    , offset : Int
    }


{-| Represents the root of the section tree
-}
type Root a
    = Root (Section (Tab a))



-- ======================
-- STATE
-- ======================


tab : TabConfig msg -> (a -> Html msg) -> a -> Tab msg
tab config view content =
    Tab config (view content)


init : (Msg msg -> msg) -> Model msg
init toMsg =
    Model
        { toMsg = toMsg
        , sections =
            Just <|
                Divider
                    { id = 2
                    , orientation = Horizontal
                    , offset = 40
                    }
                    (TabGroup <| SelectedList.singleton "1")
                    (Divider
                        { id = 3
                        , orientation = Vertical
                        , offset = 60
                        }
                        (TabGroup <| SelectedList.singleton "1")
                        (TabGroup <| SelectedList.singleton "2")
                    )
        , lastDividerId = 0
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
    Model { model | sections = Just <| sectionMap tabId section }


selectTab : TabId -> Model msg -> Model msg
selectTab id (Model model) =
    Model { model | sections = model.sections |> Maybe.map (sectionSelect id) }


tabId : Tab a -> TabId
tabId (Tab { id } _) =
    id



-- ======================
-- SECTION
-- ======================


sectionToList : Section a -> List a
sectionToList section =
    case section of
        TabGroup tabs ->
            SelectedList.toList tabs

        Divider _ s1 s2 ->
            List.concat [ sectionToList s1, sectionToList s2 ]


sectionMap : (a -> b) -> Section a -> Section b
sectionMap f s =
    case s of
        TabGroup tabs ->
            TabGroup (SelectedList.map f tabs)

        Divider info s1 s2 ->
            Divider info (sectionMap f s1) (sectionMap f s2)


sectionSelect : a -> Section a -> Section a
sectionSelect elem section =
    case section of
        TabGroup tabs ->
            TabGroup <| SelectedList.select tabs elem

        Divider info s1 s2 ->
            Divider info (sectionSelect elem s1) (sectionSelect elem s2)



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
    case section of
        TabGroup tabs ->
            div
                [ class "tabs-tabgroup" ]
                [ tabs
                    |> SelectedList.toTupleList
                    |> List.map (tabHeader root)
                    |> div [ class "tabs-tabgroup-headers" ]
                    |> Html.map toMsg
                , tabs
                    |> SelectedList.selected
                    |> viewTab
                ]

        Divider { orientation, offset } s1 s2 ->
            div
                [ classList
                    [ ( "tabs-divider", True )
                    , ( "tabs-divider--horizontal", orientation == Horizontal )
                    , ( "tabs-divider--vertical", orientation == Vertical )
                    ]
                ]
                [ division orientation offset (viewSection toMsg root s1)
                , div [ class "tabs-divider-line" ] []
                , division orientation (-offset) (viewSection toMsg root s2)
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
tabHeader root ( selected, Tab { id, title } _ ) =
    div
        [ classList
            [ ( "tabs-tabgroup-header", True )
            , ( "tabs-tabgroup-header--selected", selected )
            ]
        , onClick (SelectTab root id)
        ]
        [ text title ]


viewTab : Tab msg -> Html msg
viewTab (Tab config content) =
    div
        [ class "tabs-tabgroup-tab" ]
        [ content
        ]


{-| We have a tree structure specifying how the tabs should be laid
out int the view, but we don't know if all the tabs for which we
have ids, are still being shown. Given a tree structure with IDS
and some tabs, it will return a new tree structure with all tabs
that match the IDs passed. T
-}
toSections : Maybe (Section TabId) -> Nonempty (Tab msg) -> Section (Tab msg)
toSections mids tabs =
    let
        tabsList =
            Nonempty.toList tabs

        getTab id =
            List.Extra.find (tabId >> (==) id) tabsList

        msection =
            Maybe.andThen (fromSection getTab) mids
    in
        case msection of
            Nothing ->
                TabGroup (SelectedList.fromNonempty tabs)

            Just section ->
                let
                    sectionAsList =
                        sectionToList section

                    unordered =
                        tabs
                            |> Nonempty.toList
                            |> List.filter (not << flip List.member sectionAsList)
                            |> SelectedList.fromList
                            |> Maybe.map TabGroup
                in
                    case unordered of
                        Nothing ->
                            -- Everything is laid out in sections
                            section

                        Just usection ->
                            -- There are new tabs that still don't have
                            -- been included in the sections layout
                            Divider { id = 2, orientation = Horizontal, offset = 0 } usection section


fromSection : (TabId -> Maybe (Tab msg)) -> Section TabId -> Maybe (Section (Tab msg))
fromSection getTab section =
    case section of
        TabGroup idList ->
            case SelectedList.filterMap getTab idList of
                Nothing ->
                    Nothing

                Just tabs ->
                    Just <| TabGroup tabs

        Divider info s1 s2 ->
            case fromSection getTab s1 of
                Nothing ->
                    fromSection getTab s2

                Just content1 ->
                    case fromSection getTab s2 of
                        Nothing ->
                            Just content1

                        Just content2 ->
                            Just <| Divider info content1 content2
