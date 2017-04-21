port module Ports exposing (..)

import Http
import Task exposing (Task)
import Json.Decode as Json
import Models exposing (FileData)


port driveAuthenticate : () -> Cmd m


port driveReadFile : () -> Cmd m


port driveSaveFile : FileData -> Cmd m


port driveOnFileRead : (FileData -> m) -> Sub m


port driveOnFileReadError : (String -> m) -> Sub m


port driveOnFileSave : (() -> m) -> Sub m


port driveOnFileSaveError : (String -> m) -> Sub m


port driveOnAuthReady : (Bool -> m) -> Sub m


port driveOnAuthSystemFailure : (() -> m) -> Sub m
