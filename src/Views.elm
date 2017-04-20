module Views exposing (..)

import Models exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, colspan, disabled, placeholder, selected, style, type_, value)
import Html.Events exposing (onClick, onInput)


view : Model -> Html Msg
view model =
    case model.authState of
        NotAuthenticated ->
            div
                []
                [ button
                    [ onClick Authenticate
                    ]
                    [ text "Authenticate"
                    ]
                ]

        _ ->
            div
                []
                [ case model.state of
                    Fresh ->
                        text "Starting..."

                    Loading ->
                        text "Loading..."

                    Loaded ->
                        viewLoaded model

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
                ]


viewLoaded : Model -> Html Msg
viewLoaded model =
    case model.runData of
        Just runData ->
            viewRunData runData

        Nothing ->
            div
                []
                ([ h1
                    []
                    [ text "Your tables"
                    ]
                 , button
                    [ onClick <| CreateTable True
                    ]
                    [ text "Add O2 table"
                    ]
                 , button
                    [ onClick <| CreateTable False
                    ]
                    [ text "Add CO2 table"
                    ]
                 ]
                    ++ (if model.dirty then
                            [ button
                                [ onClick Save
                                ]
                                [ text "Save changes"
                                ]
                            , button
                                [ onClick Reload
                                ]
                                [ text "Reload from drive"
                                ]
                            ]
                        else
                            []
                       )
                    ++ (if List.isEmpty model.tables then
                            [ div
                                []
                                [ text "No tables defined"
                                ]
                            ]
                        else
                            List.indexedMap viewTable model.tables
                       )
                )


viewTable : Int -> TableDef -> Html Msg
viewTable tableIndex t =
    div
        []
        [ hr [] []
        , h3
            []
            [ input
                [ value t.name
                , onInput <| UpdateTableName tableIndex
                ]
                []
            , text <|
                if t.isO2 then
                    "(O2)"
                else
                    "(CO2)"
            , button
                [ onClick <| RemoveTable tableIndex
                ]
                [ text "Remove"
                ]
            ]
        , div
            []
            [ text <| "Duration : " ++ (formatTimeInterval <| totalDuration t)
            ]
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
                                    [ text <| toString (index + 1)
                                    ]
                                , td
                                    []
                                    [
                                        if t.isO2 then
                                            viewDuration (UpdateTableField tableIndex index False) holdTime
                                        else
                                            viewDuration (UpdateTableField tableIndex index True) t.fixed
                                    ]
                                , td
                                    []
                                    [ if index < List.length t.steps - 1 then
                                        if t.isO2 then
                                            viewDuration (UpdateTableField tableIndex index True) t.fixed
                                        else
                                            viewDuration (UpdateTableField tableIndex index False) holdTime
                                      else
                                        text ""
                                    ]
                                , td
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
        , button
            [ onClick <| RunTable tableIndex
            ]
            [ text "Run this table"
            ]
        ]


viewDuration : (String -> Msg) -> Int -> Html Msg
viewDuration msg seconds =
    input
        [ value <| toString seconds
        , onInput msg
        ]
        []


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