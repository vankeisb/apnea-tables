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
        , div
            []
            (
                if model.dirty then
                    [ button
                        [ onClick Save
                        ]
                        [ text "Save changes"
                        ]
                    ]
                else
                    []
            )
        , h2    
            []
            [ text "O2" 
            ]
        , button
            [ onClick <| CreateTable True
            ]
            [ text "Add O2 table"
            ]
        ] ++
        (
            if List.isEmpty model.o2Tables then
                [ div
                    []
                    [ text "No O2 tables defined"
                    ]
                ]
            else
                List.map viewO2Table model.o2Tables
        )
        )


viewO2Table : O2TableDef -> Html Msg
viewO2Table t =
    div
        []
        [ h3
            []
            [ input
                [ value t.name
                ]
                []
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
                ( t.holds
                    |> List.indexedMap (\index holdTime ->
                        tr
                            []
                            [ th
                                []
                                [ text <| toString index
                                ]
                            ,td
                                []
                                [ viewDuration holdTime
                                ]
                            , td
                                []
                                [ viewDuration t.breath
                                ]
                            , td
                                []
                                [ button
                                    [ onClick <| RemoveStepO2 t.name index
                                    ]
                                    [ text "Remove" ]
                                , button
                                    [ onClick <| AddStepO2 True t.name index
                                    ]
                                    [ text "Add before" ]
                                , button
                                    [ onClick <| AddStepO2 False t.name index
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