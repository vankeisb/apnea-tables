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
        ] ++
            (
            if model.dirty then
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
            ) ++
            (
            if List.isEmpty model.tables then
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
viewTable index t =
    div
        []
        [ hr [] []
        , h3
            []
            [ input
                [ value t.name
                ]
                []
            , text <|
                if t.isO2 then
                    "(O2)"
                else
                    "(CO2)"
            , button
                [ onClick <| RemoveTable index
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
                        [ text "step"]
                    ,th
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
                ( t.steps
                    |> List.indexedMap (\index holdTime ->
                        tr
                            []
                            [ th
                                []
                                [ text <| toString (index + 1)
                                ]
                            ,td
                                []
                                [ viewDuration <|
                                    if t.isO2 then
                                        holdTime
                                    else
                                        t.fixed
                                ]
                            , td
                                []
                                [
                                    if index < List.length t.steps - 1 then
                                        viewDuration <|
                                            if t.isO2 then
                                                t.fixed
                                            else
                                                holdTime
                                    else
                                        text ""
                                ]
                            , td
                                []
                                [ button
                                    [ onClick <| RemoveStep t.name index
                                    ]
                                    [ text "Remove" ]
                                , button
                                    [ onClick <| AddStep True t.name index
                                    ]
                                    [ text "Add before" ]
                                , button
                                    [ onClick <| AddStep False t.name index
                                    ]
                                    [ text "Add after" ]
                                ]
                            ]
                    )
                )
            ]
        ]


viewDuration : Int -> Html Msg
viewDuration seconds =
    input
        [ value <| toString seconds ]
        []