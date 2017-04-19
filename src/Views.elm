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
        t =
            runData.table

        (curStepIndex, curStepHold, curStepPercent) =
            case runData.progressInfo of
                Just pi ->
                    (pi.curStepIndex, pi.curStepHold, pi.curStepPercent)
                Nothing ->
                    (-1, True, 0)



        rows =
            runData.table.steps
                |> List.indexedMap (\index step ->
                    let
                        rowClass =
                            if curStepIndex < index then
                                " past"
                            else if curStepIndex == index then
                                " current"
                            else
                                " future"
                    in
                        tr
                            [ class <| "tbl-row" ++ rowClass
                            ]
                            [ th
                                []
                                [ text <| toString (index + 1)
                                ]
                            , td
                                []
                                [ text <| toString <|
                                    if t.isO2 then
                                        step
                                    else
                                        t.fixed
                                ]
                            , td
                                []
                                [ if index < List.length t.steps - 1 then
                                    text <| toString <|
                                        if t.isO2 then
                                            t.fixed
                                        else
                                            step
                                  else
                                    text ""
                                ]
                            , td
                                []
                                [
                                    if curStepIndex == index then
                                        viewProgress curStepHold curStepPercent
                                    else
                                        text ""
                                ]
                            ]
                )
    in
        div
            []
            [ h1
                []
                [ text <| runData.table.name ]
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
            , button
                [ onClick StartStopTable
                ]
                [ text <|
                    if runData.progressInfo == Nothing then
                        "Start"
                    else
                        "Stop"
                ]
            , hr [] []
            , button
                [ onClick BackToHome
                ]
                [ text "Back to home"
                ]
            ]


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
            [ text <|
                if hold then
                    "hold"
                else
                    "breathe"
            ]
        ]