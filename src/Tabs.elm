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
        , sections : Maybe (Section ID)
        }


type Msg msg
    = SelectTab (Root msg) ID


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


type Root a
    = Root (Section (Tab a))


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
                    Horizontal
                    40
                    (TabGroup <| SelectedList.singleton "1")
                    (Divider
                        Vertical
                        60
                        (TabGroup <| SelectedList.singleton "1")
                        (TabGroup <| SelectedList.singleton "2")
                    )
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


selectTab : ID -> Model msg -> Model msg
selectTab id (Model model) =
    Model { model | sections = model.sections |> Maybe.map (sectionSelect id) }


tabId : Tab a -> ID
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

        Divider _ _ s1 s2 ->
            List.concat [ sectionToList s1, sectionToList s2 ]


sectionMap : (a -> b) -> Section a -> Section b
sectionMap f s =
    case s of
        TabGroup tabs ->
            TabGroup (SelectedList.map f tabs)

        Divider orientation percentage s1 s2 ->
            Divider orientation percentage (sectionMap f s1) (sectionMap f s2)


sectionSelect : a -> Section a -> Section a
sectionSelect elem section =
    case section of
        TabGroup tabs ->
            TabGroup <| SelectedList.select tabs elem

        Divider orientation percentage s1 s2 ->
            Divider orientation percentage (sectionSelect elem s1) (sectionSelect elem s2)



-- ======================
-- VIEW
-- ======================


view : Model msg -> Nonempty (Tab msg) -> Html msg
view (Model model) tabs =
    tabs
        |> toLayout (Debug.log "sections" model.sections)
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

        Divider orientation percentage s1 s2 ->
            div
                [ classList
                    [ ( "tabs-divider", True )
                    , ( "tabs-divider--horizontal", orientation == Horizontal )
                    , ( "tabs-divider--vertical", orientation == Vertical )
                    ]
                ]
                [ divider orientation percentage (viewSection toMsg root s1)
                , div [ class "tabs-divider-line" ] []
                , divider orientation (100 - percentage) (viewSection toMsg root s2)
                ]


pc : Int -> String
pc v =
    toString v ++ "%"


divider : Orientation -> Int -> Html msg -> Html msg
divider orientation percentage content =
    div
        [ class "tabs-divider-division"
        , style
            [ ( case orientation of
                    Horizontal ->
                        "width"

                    Vertical ->
                        "height"
              , pc percentage
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
toLayout : Maybe (Section ID) -> Nonempty (Tab msg) -> Section (Tab msg)
toLayout mids tabs =
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
                            Divider Horizontal 50 usection section


fromSection : (ID -> Maybe (Tab msg)) -> Section ID -> Maybe (Section (Tab msg))
fromSection getTab section =
    case section of
        TabGroup idList ->
            case SelectedList.filterMap getTab idList of
                Nothing ->
                    Nothing

                Just tabs ->
                    Just <| TabGroup tabs

        Divider orientation percentage s1 s2 ->
            case fromSection getTab s1 of
                Nothing ->
                    fromSection getTab s2

                Just content1 ->
                    case fromSection getTab s2 of
                        Nothing ->
                            Just content1

                        Just content2 ->
                            Just <| Divider orientation percentage content1 content2
