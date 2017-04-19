module Writer exposing (..)

import Models exposing (..)

import Json.Encode exposing (..)


encodeTableDef : TableDef -> Value
encodeTableDef t =
    object
        [ ("name", string t.name)
        , ("isO2", bool t.isO2)
        , ("fixed", int t.fixed)
        , ("steps", list <| List.map int t.steps)
        ]


encodeSerializedData : SerializedData -> Value
encodeSerializedData sd =
    object
        [ ("tables", list <| List.map encodeTableDef sd.tables)
        ]