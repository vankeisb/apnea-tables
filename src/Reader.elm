module Reader exposing (..)

import Date exposing (Date)
import Models exposing (..)
import Json.Decode exposing (..)


serializedDataDecoder : Decoder SerializedData
serializedDataDecoder =
    map SerializedData
        (field "tables" <| list tableDecoder)


tableDecoder : Decoder TableDef
tableDecoder =
    map4 TableDef
        (field "name" string)
        (field "isO2" bool)
        (field "fixed" int)
        (field "steps" <| list int)


