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
             let
                 serializedData =
                     Json.Decode.decodeString Reader.serializedDataDecoder res.content
             in
                 case serializedData of
                     Ok data ->
                         ( { model
                             | tables = data.tables
                             , state = Loaded
                             , fileId = res.fileId
                         }
                         , Cmd.none
                         )
                     Err e ->
                         ({ model
                             | state = LoadError e
                         }
                         , Cmd.none
                         )


        ReadFileError err ->
            ({ model
                | state = LoadError err
            }
            , Cmd.none
            )


        Reload ->
            ( { model
                | state = Loading
                , dirty = False
            }
            , Drive.driveReadFile ()
            )


        Save ->
            let
                fileContents =
                    { tables = model.tables }
                        |> Writer.encodeSerializedData
                        |> Json.Encode.encode 4

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
                , dirty = False
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
                c =
                    model.counter
                        |> toString

                tableName =
                    "New table " ++ c

                table =
                    if isO2 then
                        { name = tableName
                        , isO2 = True
                        , fixed = 120
                        , steps =
                            ( List.range 1 7
                                |> List.map (\i -> (i * 15) + 30)
                            ) ++ [ 135 ]
                        }
                    else
                        { name = tableName
                        , isO2 = False
                        , fixed = 120
                        , steps =
                            List.range 1 8
                                |> List.map (\i -> (i * 15) + 15)
                                |> List.reverse
                        }
            in
                ({ model
                    | tables =
                        table :: model.tables
                    , dirty = True
                    , counter = model.counter + 1
                }
                , Cmd.none
                )


        RemoveTable index ->
            let
                newTables =
                    model.tables
                        |> removeFromList index
            in
                ({model
                    | tables = newTables
                    , dirty = True
                }
                , Cmd.none
                )


        RemoveStep tableIndex stepIndex ->
            ( replaceTable
                model
                tableIndex
                (\t ->
                    { t
                        | steps =
                            t.steps
                                |> removeFromList stepIndex
                    }
                )
            , Cmd.none
            )

        AddStep before tableIndex stepIndex ->
            ( replaceTable
                model
                tableIndex
                (\t ->
                    { t
                        | steps =
                            t.steps
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

        UpdateTableName index newName ->
            ( replaceTable
                model
                index
                (\t -> { t | name = newName })
            , Cmd.none
            )


replaceTable : Model -> Int -> (TableDef -> TableDef) -> Model
replaceTable model tableIndex mapper =
    let
        newTables =
            model.tables
                |> List.indexedMap (\index t ->
                    if tableIndex == index then
                        mapper t
                    else
                        t
                )
    in
        { model
            | tables = newTables
            , dirty = True
        }


removeFromList i xs =
    (List.take i xs) ++ (List.drop (i+1) xs)


insertIntoListAt i x xs =
    (List.take i xs) ++ [x] ++ (List.drop (i) xs)