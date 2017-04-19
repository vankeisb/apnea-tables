module App exposing (..)

import Drive
import Json.Encode
import Models exposing (..)
import Time
import Views as Views
import Update
import Html exposing (..)
import Date exposing (..)
import Task
import Writer


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Drive.driveOnFileRead ReadFileOk
        , Drive.driveOnFileReadError ReadFileError
        , Drive.driveOnAuthReady AuthReady
        , Drive.driveOnFileSave (\() -> SaveOk)
        , Drive.driveOnFileSaveError SaveError
        ,
            case model.runData of
                Just runData ->
                    runData.progressInfo
                        |> Maybe.map (\pi -> Time.every Time.second Tick)
                        |> Maybe.withDefault Sub.none
                Nothing ->
                    Sub.none
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = Views.view
        , update = Update.update
        , subscriptions = subscriptions
        }
