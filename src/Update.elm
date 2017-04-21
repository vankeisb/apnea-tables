module Update exposing (..)

import Date
import Ports
import Json.Decode
import Json.Encode
import Material
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
            ( model, Ports.driveAuthenticate () )

        AuthReady authenticated ->
            ( { model
                | authState =
                    if authenticated then
                        Authenticated
                    else
                        NotAuthenticated
                , state =
                    if model.state == Fresh then
                        Ready
                    else
                        model.state
              }
            , Cmd.none
            )

        AuthSystemFailure ->
            ( { model
                | authState = AuthSystemFailed
                , state = Ready
              }
            , Cmd.none
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
                        ( { model
                            | state = LoadError e
                          }
                        , Cmd.none
                        )

        ReadFileError err ->
            ( { model
                | state = LoadError err
              }
            , Cmd.none
            )

        Reload ->
            ( { model
                | state = Loading
                , dirty = False
              }
            , Ports.driveReadFile ()
            )

        Save ->
            let
                fileContents =
                    { tables = model.tables }
                        |> Writer.encodeSerializedData
                        |> Json.Encode.encode 4

                saveCmd =
                    Ports.driveSaveFile
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
                table =
                    if isO2 then
                        { name = "New table"
                        , isO2 = True
                        , fixed = 120
                        , steps =
                            (List.range 1 7
                                |> List.map (\i -> (i * 15) + 30)
                            )
                                ++ [ 135 ]
                        }
                    else
                        { name = "New table"
                        , isO2 = False
                        , fixed = 120
                        , steps =
                            List.range 1 8
                                |> List.map (\i -> (i * 15) + 15)
                                |> List.reverse
                        }
            in
                ( { model
                    | tables =
                        table :: model.tables
                    , dirty = True
                  }
                , Cmd.none
                )

        RemoveTable index ->
            let
                newTables =
                    model.tables
                        |> removeFromList index
            in
                ( { model
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

        UpdateTableField tableIndex index isFixed newValue ->
            ( replaceTable
                model
                tableIndex
                (\t ->
                    let
                        newValueInt =
                            String.toInt newValue
                                |> Result.withDefault 0
                    in
                        if isFixed then
                            { t | fixed = newValueInt }
                        else
                            { t
                                | steps =
                                    t.steps
                                        |> List.indexedMap
                                            (\stepIndex step ->
                                                if stepIndex == index then
                                                    newValueInt
                                                else
                                                    step
                                            )
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
                                    (if before then
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

        RunTable index ->
            let
                table =
                    model.tables
                        |> getFromListAt index
            in
                case table of
                    Just t ->
                        ( { model
                            | runData = Just <| initRunData t
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        model ! []

        Tick time ->
            withRunData model
                (\runData ->
                    case runData.startTime of
                        Just startTime ->
                            let
                                elapsed =
                                    time - startTime

                                findStepIndex index steps total =
                                    case steps of
                                        first :: rest ->
                                            let
                                                stepLen =
                                                    (runData.table.fixed + first) * 1000

                                                stepStart =
                                                    total

                                                stepEnd =
                                                    total + stepLen
                                            in
                                                if elapsed >= toFloat stepStart && elapsed < toFloat stepEnd then
                                                    let
                                                        stepHoldEnd =
                                                            total
                                                                + if runData.table.isO2 then
                                                                    first * 1000
                                                                  else
                                                                    runData.table.fixed * 1000

                                                        hold =
                                                            elapsed < toFloat stepHoldEnd

                                                        stepDuration =
                                                            toFloat <|
                                                                if hold then
                                                                    stepHoldEnd - stepStart
                                                                else
                                                                    stepEnd - stepHoldEnd

                                                        stepElapsed =
                                                            if hold then
                                                                elapsed - (toFloat stepStart)
                                                            else
                                                                elapsed - (toFloat stepHoldEnd)

                                                        percent =
                                                            stepElapsed * 100 / stepDuration

                                                        isLastStep =
                                                            List.isEmpty rest

                                                        isCompleted =
                                                            isLastStep && not hold
                                                    in
                                                        ( index, hold, round percent, isCompleted )
                                                else
                                                    findStepIndex (index + 1) rest (total + stepLen)

                                        [] ->
                                            ( -1, True, 0, False )

                                ( newStepIndex, newStepHold, newStepPercent, completed ) =
                                    findStepIndex 0 runData.table.steps 0
                            in
                                ( replaceRunData
                                    model
                                    { runData
                                        | curTime = time
                                        , curStepIndex = newStepIndex
                                        , curStepHold = newStepHold
                                        , curStepPercent = newStepPercent
                                        , completed = completed
                                    }
                                , Cmd.none
                                )

                        Nothing ->
                            ( replaceRunData
                                model
                                { runData
                                    | curTime = time
                                }
                            , Cmd.none
                            )
                )

        BackToHome ->
            ( { model
                | runData = Nothing
              }
            , Cmd.none
            )

        StartStopClicked ->
            withRunData model
                (\runData ->
                    if isStarted runData && (not <| isStoppedOrCompleted runData) then
                        ( replaceRunData
                            model
                            { runData
                                | stopTime = Just runData.curTime
                            }
                        , Cmd.none
                        )
                    else
                        ( model
                        , Time.now
                            |> Task.perform StartTable
                        )
                )

        StartTable time ->
            withRunData model
                (\runData ->
                    let
                        newRunData =
                            initRunData runData.table
                    in
                        ( replaceRunData
                            model
                            { newRunData
                                | startTime = Just time
                            }
                        , Cmd.none
                        )
                )

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model


replaceRunData model runData =
    { model | runData = Just runData }


withRunData : Model -> (RunData -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
withRunData model f =
    model.runData
        |> Maybe.map f
        |> Maybe.withDefault ( model, Cmd.none )


replaceTable : Model -> Int -> (TableDef -> TableDef) -> Model
replaceTable model tableIndex mapper =
    let
        newTables =
            model.tables
                |> List.indexedMap
                    (\index t ->
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
    (List.take i xs) ++ (List.drop (i + 1) xs)


insertIntoListAt i x xs =
    (List.take i xs) ++ [ x ] ++ (List.drop (i) xs)


getFromListAt i xs =
    (List.drop i xs)
        |> List.head
