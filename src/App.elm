module App exposing (..)

import Ports
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
        [ Ports.driveOnFileRead ReadFileOk
        , Ports.driveOnFileReadError ReadFileError
        , Ports.driveOnAuthReady AuthReady
        , Ports.driveOnFileSave (\() -> SaveOk)
        , Ports.driveOnFileSaveError SaveError
        , Ports.driveOnAuthSystemFailure (\() -> AuthSystemFailure)
        , case model.runData of
            Just runData ->
                if needsTick runData then
                    Time.every Time.second Tick
                else
                    Sub.none

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
