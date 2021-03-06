module Views exposing (..)

import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, colspan, disabled, placeholder, selected, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Material.Button as Button
import Material.Options as Options
import Material.Layout as Layout
import Material.Textfield as Textfield
import Material.Table as Table
import Material.Menu as Menu
import Material.Icon as Icon
import Material.Card as Card
import Material.Color as Color
import Material.Typography as Typography
import Material.Elevation as Elevation
import Material.Progress as Prg

padding =
    Options.css "padding-right" "24px"


viewBanner : Model -> Html Msg
viewBanner model =
    let
        i name =
            Icon.view name [ Options.css "width" "40px" ]

        authItem =
            Menu.item
                [ Menu.onSelect <| Authenticate, padding ]
                [ i "account_circle", text "Authenticate" ]

        loadItem =
            Menu.item
                [ Menu.onSelect <| Reload, padding ]
                [ i "cloud_download", text "Load from drive" ]

        saveItem =
            Menu.item
                [ Menu.onSelect <| Save, padding ]
                [ i "cloud_upload", text "Save" ]

        addO2Item =
            Menu.item
                [ Menu.onSelect <| CreateTable True, padding ]
                [ i "playlist_add", text "O2 table" ]

        addCO2Item =
            Menu.item
                [ Menu.onSelect <| CreateTable False, padding ]
                [ i "playlist_add", text "CO2 table" ]

        addItems =
            [ addO2Item, addCO2Item ]

        menuItems =
            case model.authState of
                Authenticated ->
                    ([ loadItem
                     ]
                        ++ if model.dirty then
                            [ saveItem
                            ]
                           else
                            []
                    )
                        ++ addItems

                AuthSystemFailed ->
                    addItems

                _ ->
                    [ authItem ] ++ addItems
    in
        Layout.row
            [ Options.css "padding-left" "16px"
            , Options.css "padding-right" "16px"
            ]
            ([ Layout.title
                []
                [ text "Apnea tables"
                ]
             , Layout.spacer
             ]
                ++ [ case model.state of
                        Fresh ->
                            text ""

                        Ready ->
                            text ""

                        Loading ->
                            text "Loading..."

                        Loaded ->
                            (if model.dirty then
                                text ""
                             else
                                text "Sync-ed with drive"
                            )

                        LoadError err ->
                            text "Load error !!"

                        Saving ->
                            text "Saving..."

                        SavingError err ->
                            div
                                []
                                [ h3
                                    []
                                    [ text "Saving error" ]
                                , p
                                    []
                                    [ text err
                                    ]
                                ]
                   ]
                ++ [ Menu.render Mdl
                        [ 3 ]
                        model.mdl
                        [ Menu.ripple, Menu.bottomRight ]
                        menuItems
                   ]
            )


view : Model -> Html Msg
view model =
    case model.state of
        Fresh ->
            text "Starting..."

        _ ->
            case model.runData of
                Just runData ->
                    viewRunData model runData

                Nothing ->
                    Layout.render Mdl
                        model.mdl
                        [ Layout.fixedHeader
                        ]
                        { header = [ viewBanner model ]
                        , drawer = []
                        , tabs = ( [], [] )
                        , main =
                            [ div
                                [ style
                                    [ ("padding", "2em")
                                    ]
                                ]
                                (if List.isEmpty model.tables then
                                    [ Options.styled p
                                        [ Typography.title
                                        ]
                                        [ text "No tables found" ]
                                    , Options.styled p
                                        [ Typography.body1
                                        ]
                                        [ text """You have no tables defined.
                                                Create tables, or load from google drive
                                                using the menu.""" ]
                                    ]
                                 else
                                    List.indexedMap (viewTable model) model.tables
                                )
                            ]
                        }


displayFlex =
    Options.css "display" "flex"


flexGrow =
    Options.css "flex-grow" "1"


alignItemsInCenter =
    Options.css "align-items" "center"


flexRow =
    Options.css "flex-direction" "row"


flexColumn =
    Options.css "flex-direction" "column"

white : Options.Property c m
white =
    Color.text Color.white


viewTable : Model -> Int -> TableDef -> Html Msg
viewTable model tableIndex t =
    Card.view
        [ Options.css "width" "100%"
        , Elevation.e4
        , Options.css "margin-bottom" "2em"
        , Options.cs "tbl-card"
        ]
        [ Card.title
            [ displayFlex, flexRow ]
            [ Card.head
                [ displayFlex, flexColumn, flexGrow ]
                [ Options.div
                    [ displayFlex, flexGrow, flexRow, alignItemsInCenter ]
                    [ Options.div
                        [ flexGrow ]
                        [ Textfield.render Mdl
                            [ 2, tableIndex, 0 ]
                            model.mdl
                            [ Textfield.label "Table name"
                            , Textfield.value t.name
                            , Textfield.text_
                            , Options.onInput <| UpdateTableName tableIndex
                            , Options.css "width" "100%"
                            , Options.cs "tbl-name"
                            ]
                            []
                        ]
                    , Options.div
                        []
                        [ Button.render Mdl
                            [ 3, tableIndex, 0 ]
                            model.mdl
                            [ Button.icon
                            , Button.ripple
                            , Options.onClick <| RemoveTable tableIndex
                            ]
                            [ Icon.i "close" ]
                        ]
                    ]
                , Options.div
                    [ displayFlex
                    , flexGrow
                    , flexRow
                    , alignItemsInCenter
                    , Options.css "margin-bottom" "1em"
                    ]
                    [ Options.div
                        [ flexGrow ]
                        [ text <|
                            (if t.isO2 then
                                "O2"
                             else
                                "CO2"
                            )
                                ++ (" - " ++ (totalDuration t |> formatTimeInterval))
                        ]
                    , Options.div
                        []
                        [ Button.render Mdl
                            [ 3, tableIndex, 1 ]
                            model.mdl
                            [ Button.colored
                            , Button.fab
                            , Button.ripple
                            , Button.icon
                            , Options.onClick <| RunTable tableIndex
                            ]
                            [ Icon.i "play_arrow" ]
                        ]
                    ]
                ]
            ]
        , Card.text
            [ Options.css "width" "100%"
            , Options.css "padding" "0"
            ]
            [ table
                [ class "tbl-steps"
                ]
                [ thead
                    []
                    [ tr
                        []
                        [ th
                            []
                            []
                        , th
                            []
                            [ text "hold"
                            ]
                        , th
                            []
                            [ text "breathe"
                            ]
                        , th
                            []
                            []
                        ]
                    ]
                , tbody
                    []
                    (t.steps
                        |> List.indexedMap
                            (\index holdTime ->
                                tr
                                    []
                                    [ th
                                        []
                                        [ text <| "#" ++ (toString (index + 1))
                                        ]
                                    , td
                                        []
                                        [ if t.isO2 then
                                            viewDuration model tableIndex index False holdTime
                                          else
                                            viewDuration model tableIndex index True t.fixed
                                        ]
                                    , td
                                        []
                                        [ if index < List.length t.steps - 1 then
                                            if t.isO2 then
                                                viewDuration model tableIndex index True t.fixed
                                            else
                                                viewDuration model tableIndex index False holdTime
                                          else
                                            text ""
                                        ]
                                    , td
                                        []
                                        [ Button.render Mdl
                                            [ 3, tableIndex, index, 0 ]
                                            model.mdl
                                            [ Button.icon
                                            , Button.ripple
                                            , Options.onClick <| RemoveStep tableIndex index
                                            ]
                                            [ Icon.i "close" ]
                                        , Button.render Mdl
                                            [ 3, tableIndex, index, 1 ]
                                            model.mdl
                                            [ Button.icon
                                            , Button.ripple
                                            , Options.onClick <| AddStep True tableIndex index
                                            ]
                                            [ Icon.i "arrow_upward" ]
                                        , Button.render Mdl
                                            [ 3, tableIndex, index, 2 ]
                                            model.mdl
                                            [ Button.icon
                                            , Button.ripple
                                            , Options.onClick <| AddStep False tableIndex index
                                            ]
                                            [ Icon.i "arrow_downward" ]
                                        ]
                                    ]
                            )
                    )
                ]
            ]
        ]


viewDuration : Model -> Int -> Int -> Bool -> Int -> Html Msg
viewDuration model tableIndex stepIndex isFixed seconds =
    Textfield.render Mdl
        [ 3
        , tableIndex
        , stepIndex + 1
        , if isFixed then
            0
          else
            1
        ]
        model.mdl
        [ Textfield.label "Duration (seconds)"
        , Textfield.value <| toString seconds
        , Textfield.text_
        , Options.onInput <| UpdateTableField tableIndex stepIndex isFixed
        ]
        []


viewRunData : Model -> RunData -> Html Msg
viewRunData model runData =
    let
        { curStepIndex, curStepHold, curStepPercent, completed } =
            runData

        t =
            runData.table

        rows =
            t.steps
                |> List.indexedMap
                    (\index step ->
                        let
                            p =
                                "past"

                            c =
                                "current"

                            f =
                                "future"

                            ( timeClass, holdClass, breatheClass ) =
                                if completed then
                                    ( p, p, p )
                                else if curStepIndex < index then
                                    ( f, f, f )
                                else if curStepIndex == index then
                                    ( c
                                    , if curStepHold then
                                        c
                                      else
                                        p
                                    , if curStepHold then
                                        f
                                      else
                                        c
                                    )
                                else
                                    ( p, p, p )
                        in
                            tr
                                [ class "tbl-row"
                                ]
                                [ th
                                    [ class timeClass ]
                                    [ text <| "#" ++ (toString (index + 1))
                                    ]
                                , td
                                    [ class holdClass ]
                                    [ text <|
                                        formatSeconds <|
                                            if t.isO2 then
                                                step
                                            else
                                                t.fixed
                                    ]
                                , td
                                    [ class breatheClass ]
                                    [ if index < List.length t.steps - 1 then
                                        text <|
                                            formatSeconds <|
                                                if t.isO2 then
                                                    t.fixed
                                                else
                                                    step
                                      else
                                        text "-"
                                    ]
                                , td
                                    []
                                    [ if not completed && curStepIndex == index then
                                        viewProgress curStepHold curStepPercent
                                      else
                                        viewProgressEmpty
                                    ]
                                ]
                    )


        btnStartStop =
            Button.render Mdl
                [ 101 ]
                model.mdl
                [ Button.fab
                , Button.colored
                , Button.ripple
                , Button.icon
                , Options.onClick StartStopClicked
                ]
                [ Icon.i <|
                    if isStarted runData && (not <| isStoppedOrCompleted runData) then
                        "stop"
                    else
                        "play_arrow"
                ]


        headElems =
            if runData.completed then
                [ Options.styled p
                    [ Typography.title
                    , Options.css "margin-top" "1em"
                    ]
                    [ text "Congrats !" ]
                , Options.styled p
                    [ Typography.body1
                    ]
                    [ text "You made it ! Try a harder table..." ]
                ]
            else if runData.stopTime /= Nothing then
                [ Options.styled p
                    [ Typography.title
                    ]
                    [ text "Table stopped" ]
                , Options.styled p
                    [ Typography.body1
                    ]
                    [ text "You have stopped the table before the end." ]
                ]
            else if isStarted runData then
                []
            else
                [ Options.styled p
                    [ Typography.title
                    ]
                    [ text "Ready !" ]
                , Options.styled p
                    [ Typography.body1
                    ]
                    [ text "Click the 'play' button whenever you are ready." ]
                ]

    in
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header =
                ([ Layout.row
                    [ Options.css "padding-left" "8px" ]
                    [ Button.render Mdl
                        [ 100 ]
                        model.mdl
                        [ Button.icon
                        , Options.onClick BackToHome
                        ]
                        [ Icon.i "arrow_back" ]
                    , Layout.title
                        []
                        [ text <|
                            t.name
                                ++ if t.isO2 then
                                    " (O2)"
                                   else
                                    " (CO2)"
                        ]
                    ]
                ] ++
                    if isStarted runData && (not <| isStoppedOrCompleted runData) then
                        [ Prg.indeterminate ]
                    else
                        []
                )
            , drawer = []
            , tabs = ( [], [] )
            , main =
                [ Options.div
                    [ Options.cs "tbl-run-container"
                    ]
                    [ div
                        [ class "tbl-run-head" ]
                        headElems
                    , btnStartStop
                    , table
                        [ class "tbl-run"
                        ]
                        [ thead
                            []
                            [ tr
                                []
                                [ th
                                    []
                                    []
                                , th
                                    []
                                    [ text "hold"
                                    ]
                                , th
                                    []
                                    [ text "breathe"
                                    ]
                                , th
                                    []
                                    [ text "progress" ]
                                ]
                            ]
                        , tbody
                            []
                            rows
                        ]
                    ]
                ]
            }


viewProgressEmpty =
    div
        [ class "progress"
        ]
        []


viewProgress : Bool -> Int -> Html Msg
viewProgress hold percent =
    div
        [ class <| "progress" ++
            if hold then
                " hold"
            else
                " breathe"
        ]
        [ Prg.progress <| toFloat percent
        ]
