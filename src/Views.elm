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


viewBanner : Model -> Html Msg
viewBanner model =
    let
        loadBtn =
            Button.render Mdl [0] model.mdl
               [ Button.raised
               , Options.onClick Reload
               ]
               [ text "Load from Drive"]

        saveBtn =
            Button.render Mdl [1] model.mdl
               [ Button.raised
               , Options.onClick Save
               ]
               [ text "Save changes to Drive"]

        authBtn =
            Button.render Mdl [2] model.mdl
               [ Button.raised
               , Options.onClick Authenticate
               ]
               [ text "Authenticate to load/save from drive"]


        i name =
            Icon.view name [ Options.css "width" "40px" ]

        padding =
            Options.css "padding-right" "24px"


        authItem =
            Menu.item
                [ Menu.onSelect <| Authenticate, padding ]
                [ i "account_circle", text "Authenticate" ]

        loadItem =
            Menu.item
                [ Menu.onSelect <| Reload, padding ]
                [ i "cloud_download", text "Load" ]

        saveItem =
            Menu.item
                [ Menu.onSelect <| Authenticate, padding ]
                [ i "cloud_upload", text "Save" ]

        addO2Item =
            Menu.item
                [ Menu.onSelect <| CreateTable True, padding ]
                [ i "playlist_add", text "O2" ]

        addCO2Item =
            Menu.item
                [ Menu.onSelect <| CreateTable False, padding ]
                [ i "playlist_add", text "CO2" ]

        addItems =
            [ addO2Item, addCO2Item ]

        menuItems =
            case model.authState of
                Authenticated ->
                    ([ loadItem
                    ] ++
                        if model.dirty then
                            [ saveItem
                            ]
                        else
                            []
                    ) ++
                    addItems
                _ ->
                    [ authItem ] ++ addItems
    in
        Layout.row
            []
            ([ Layout.title
                []
                [ text "Apnea"
                ]
            , Layout.spacer
            ] ++
            [ case model.state of
                Fresh ->
                    text ""

                Ready ->
                    text ""

                Loading ->
                    text "Loading..."

                Loaded ->
                    (
                        if model.dirty then
                            text ""
                        else
                            text "Sync-ed with drive"
                    )

                LoadError err ->
                    div
                        []
                        [ h3
                            []
                            [ text "Load error" ]
                        , p
                            []
                            [ text err
                            ]
                        ]

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
            ] ++
            [ Menu.render Mdl [4] model.mdl
                [ Menu.ripple, Menu.bottomRight ]
                menuItems
            ])



view : Model -> Html Msg
view model =
    case model.state of
        Fresh ->
            text "Starting..."

        _ ->
            case model.runData of
                Just runData ->
                    viewRunData runData

                Nothing ->
                    Layout.render Mdl model.mdl
                      [ Layout.fixedHeader
                      ]
                      { header = [ viewBanner model ]
                      , drawer = []
                      , tabs = ([], [])
                      , main =
                            [ div
                                [ class "main-content" ]
                                ([ h1
                                    []
                                    [ text "Your tables"
                                    ]
                                 ] ++
                                    if List.isEmpty model.tables then
                                        [ p
                                            []
                                            [ text "You have no tables. Load from drive, or create a new table..." ]
                                        ]
                                    else
                                        List.indexedMap (viewTable model) model.tables

                                )
                            ]
                      }



displayFlex =
    ("display", "flex")

flexGrow =
    ("flex-grow", "1")

alignItemsInCenter =
    ("align-items", "center")


viewTable : Model -> Int -> TableDef -> Html Msg
viewTable model tableIndex t =
    div
        []
        [ h2
            [ class "tbl-head" ]
            [ div
                [ style
                    [ displayFlex
                    , alignItemsInCenter
                    ]
                ]
                [ div
                    [ style
                        [ flexGrow ]
                    ]
                    [ Textfield.render Mdl [2, tableIndex, 0] model.mdl
                        [ Textfield.label "Table name"
                        , Textfield.value t.name
                        , Textfield.text_
                        , Options.onInput <| UpdateTableName tableIndex
                        , Options.css "width" "100%"
                        ]
                        []
                    ]
                , div
                    []
                    [ text <|
                        (if t.isO2 then
                            " (O2 - "
                        else
                            " (CO2 - "
                        ) ++
                        (formatTimeInterval <| totalDuration t) ++
                        ")"
                    ]
                ]
            ]
        , Table.table
            [ Options.cs "tbl" ]
            [ Table.thead
                []
                [ Table.tr
                    []
                    [ Table.th
                        []
                        [ text "step" ]
                    , Table.th
                        []
                        [ text "hold"
                        ]
                    , Table.th
                        []
                        [ text "breathe"
                        ]
                    , Table.th
                        []
                        []
                    ]
                ]
            , Table.tbody
                []
                (t.steps
                    |> List.indexedMap
                        (\index holdTime ->
                            Table.tr
                                []
                                [ Table.td
                                    []
                                    [ text <| "#" ++ (toString (index + 1))
                                    ]
                                , Table.td
                                    []
                                    [
                                        if t.isO2 then
                                            viewDuration model tableIndex index False holdTime
                                        else
                                            viewDuration model tableIndex index True t.fixed
                                    ]
                                , Table.td
                                    []
                                    [ if index < List.length t.steps - 1 then
                                        if t.isO2 then
                                            viewDuration model tableIndex index True t.fixed
                                        else
                                            viewDuration model tableIndex index False holdTime
                                      else
                                        text ""
                                    ]
                                , Table.td
                                    []
                                    [ button
                                        [ onClick <| RemoveStep tableIndex index
                                        ]
                                        [ text "Remove" ]
                                    , button
                                        [ onClick <| AddStep True tableIndex index
                                        ]
                                        [ text "Add before" ]
                                    , button
                                        [ onClick <| AddStep False tableIndex index
                                        ]
                                        [ text "Add after" ]
                                    ]
                                ]
                        )
                )
            ]
        , Button.render Mdl [2, tableIndex, 1] model.mdl
            [ Button.raised
            , Options.onClick <| RemoveTable tableIndex
            ]
            [ text "Remove"]
        , Button.render Mdl [2, tableIndex, 3] model.mdl
            [ Button.raised
            , Options.onClick <| RunTable tableIndex
            ]
            [ text "Start training"]
        , hr [] []
        ]


viewDuration : Model -> Int -> Int -> Bool -> Int -> Html Msg
viewDuration model tableIndex stepIndex isFixed seconds =
    Textfield.render Mdl [3, tableIndex, stepIndex, if isFixed then 0 else 1] model.mdl
        [ Textfield.label "Duration (seconds)"
        , Textfield.value <| toString seconds
        , Textfield.text_
        , Options.onInput <| UpdateTableField tableIndex stepIndex isFixed
        , Options.css "width" "100%"
        ]
        []
--
--    input
--        [ value <| toString seconds
--        , onInput (UpdateTableField tableIndex stepIndex isFixed)
--        ]
--        []


viewRunData : RunData -> Html Msg
viewRunData runData =
    let
        { curStepIndex, curStepHold, curStepPercent, completed } =
            runData

        t =
            runData.table

        rows =
            t.steps
                |> List.indexedMap (\index step ->
                    let
                        p = "past"
                        c = "current"
                        f = "future"

                        (timeClass, holdClass, breatheClass) =
                            if completed then
                                (p, p, p)
                            else
                                if curStepIndex < index then
                                    (f, f, f)
                                else if curStepIndex == index then
                                    ( c
                                    ,
                                        if curStepHold then
                                            c
                                        else
                                            p
                                    ,
                                        if curStepHold then
                                            f
                                        else
                                            c
                                    )
                                else
                                    (p, p, p)

                    in
                        tr
                            [ class "tbl-row"
                            ]
                            [ th
                                [ class timeClass ]
                                [ text <| toString (index + 1)
                                ]
                            , td
                                [ class holdClass ]
                                [ text <| formatSeconds <|
                                    if t.isO2 then
                                        step
                                    else
                                        t.fixed
                                ]
                            , td
                                [ class breatheClass ]
                                [ if index < List.length t.steps - 1 then
                                    text <| formatSeconds <|
                                        if t.isO2 then
                                            t.fixed
                                        else
                                            step
                                  else
                                    text "-"
                                ]
                            , td
                                []
                                [
                                    if not completed && curStepIndex == index then
                                        viewProgress curStepHold curStepPercent
                                    else
                                        viewProgressEmpty
                                ]
                            ]
                )

        buttons =
            if isStoppedOrCompleted runData then
                []
            else
                if isStarted runData then
                    [ button
                        [ onClick StopClicked
                        ]
                        [ text "Stop !!!"
                        ]
                    ]
                else
                    [ button
                        [ onClick StartClicked
                        ]
                        [ text "Start"
                        ]
                    ]

        headElems =
            if runData.completed then
                [ p
                    []
                    [ text "Congrats, you made it !"
                    ]
                ]
            else
                if runData.stopTime /= Nothing then
                    [ p
                        []
                        [ text "Stopped !" ]
                    ]
                else
                    if isStarted runData then
                        [ p
                            []
                            [ text "In progress..."
                            ]
                        ]
                    else
                        []
    in
        div
            []
            [ h1
                []
                [ text <| t.name ++
                    if t.isO2 then
                        " (O2)"
                    else
                        " (CO2)"
                ]
            , div
                []
                headElems
            , table
                []
                [ thead
                    []
                    [ tr
                        []
                        [ th
                            []
                            [ text "step" ]
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
            , div
                []
                buttons
            , hr [] []
            , button
                [ onClick BackToHome
                ]
                [ text "Back to home"
                ]
            ]


viewProgressEmpty =
    div
        [ class "progress"
        ]
        []


viewProgress : Bool -> Int -> Html Msg
viewProgress hold percent =
    div
        [ class "progress"
        ]
        [ div
            [ class
                <| "progress-bar" ++
                    if hold then
                        " hold"
                    else
                        " breathe"
            , style
                [ ("width", (toString percent) ++ "%")
                ]
            ]
            [ text ""
            ]
        , div
            [ class "progress-label"
            ]
            [ text <|
                if hold then
                    "hold"
                else
                    "breathe"
            ]
        ]