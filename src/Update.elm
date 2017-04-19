module Update exposing (..)

import Date
import Drive
import Json.Decode
import Json.Encode
import Models exposing (..)
import Task
import Reader
import Time
import Writer


updateNoCmd model =
    model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of

        Authenticate -> 
            ( model, Drive.driveAuthenticate ())

        AuthReady authenticated ->                        
            let
                (newState, loadCmd) =
                    if model.authState == AuthUnknown then 
                        (Loading, Drive.driveReadFile ())
                    else 
                        (model.state, Cmd.none)
            in
                ({ model 
                    | authState = 
                        if authenticated then 
                            Authenticated 
                        else 
                            NotAuthenticated           
                    , state = newState
                }
                , loadCmd
                )

        ReadFileOk res ->
            ({ model | state = Loaded }, Cmd.none )
            -- let
            --     serializedData =
            --         Json.Decode.decodeString Reader.serializedDataDecoder res.content
            -- in
            --     case serializedData of
            --         Ok data ->
            --             ( computeAllLogs
            --                 { model
            --                     | logs = data.logs
            --                     , contractStart = data.contractStart
            --                     , state = Loaded False
            --                     , fileId = res.fileId
            --                 }
            --             , Cmd.none
            --             )
            --         Err e ->
            --             ({ model
            --                 | logs = []
            --                 , contractStart = Date.fromTime 0
            --                 , state = LoadError e
            --             }
            --             , Cmd.none
            --             )


        ReadFileError err ->
            ({ model
                | state = LoadError err
            }
            , Cmd.none
            )


        Reload ->
            ( { model
                | state = Fresh
            }
            , Drive.driveReadFile ()
            )


        Save ->
            let
                fileContents =
                    ""

                saveCmd =
                    Drive.driveSaveFile
                        { fileId = model.fileId
                        , content = fileContents
                        }
            in
                ( { model
                    | state = Saving
                }
                , saveCmd
                )


        SaveOk ->
            ( { model
                | state = Loaded
            }
            , Cmd.none
            )


        SaveError err ->
            ( { model
                | state = SavingError err
            }
            , Cmd.none
            )


        CreateTable isO2 ->
            let
                o2Table =
                    { name = "New table"
                    , holds =
                        List.range 1 8
                            |> List.map (\i -> (i * 15) + 45)
                    , breath = 120
                    }
            in
                ({ model
                    | o2Tables =
                        o2Table :: model.o2Tables
                    , dirty = True
                }
                , Cmd.none
                )


        RemoveStepO2 tableName stepIndex ->
            ( replaceO2Table
                model
                tableName
                (\t ->
                    { t
                        | holds =
                            t.holds
                                |> removeFromList stepIndex
                    }
                )
            , Cmd.none
            )

        AddStepO2 before tableName stepIndex ->
            ( replaceO2Table
                model
                tableName
                (\t ->
                    { t
                        | holds =
                            t.holds
                                |> insertIntoListAt
                                    (
                                        if before then
                                            stepIndex
                                        else
                                            stepIndex + 1
                                    )
                                    0
                    }
                )
            , Cmd.none
            )


replaceO2Table : Model -> String -> (O2TableDef -> O2TableDef) -> Model
replaceO2Table model tableName mapper =
    let
        newTables =
            model.o2Tables
                |> List.map (\t ->
                    if t.name == tableName then
                        mapper t
                    else
                        t
                )
    in
        { model
            | o2Tables = newTables
            , dirty = True
        }


removeFromList i xs =
    (List.take i xs) ++ (List.drop (i+1) xs)


insertIntoListAt i x xs =
    (List.take i xs) ++ [x] ++ (List.drop (i) xs)