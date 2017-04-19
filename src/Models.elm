module Models exposing (..)

import Time exposing (Time)


type alias FileData =
    { fileId : String
    , content : String
    }


type ModelState
    = Fresh
    | Loading
    | Loaded
    | LoadError String
    | Saving
    | SavingError String


type AuthState
    = Authenticated
    | NotAuthenticated
    | AuthUnknown


type alias RunData =
    { table : TableDef
    , curTime : Time
    , progressInfo : Maybe ProgressInfo
    }


type alias ProgressInfo =
    { startTime : Time
    , curStepIndex : Int
    , curStepHold : Bool
    , curStepPercent : Int
    }

type alias Model =
    { state : ModelState
    , fileId : String
    , authState : AuthState
    , tables : List TableDef
    , dirty : Bool
    , runData : Maybe RunData
    }


type alias TableDef =
    { name : String
    , isO2 : Bool
    , fixed : Int
    , steps : List Int
    }


initialModel : Model
initialModel =
    { state = Fresh
    , fileId = ""
    , authState = AuthUnknown
    , tables = []
    , dirty = False
    , runData = Nothing
    }


initRunData : TableDef -> RunData
initRunData table =
    { table = table
    , curTime = 0.0
    , progressInfo = Nothing
    }


type Msg
    = AuthReady Bool
    | Authenticate
    | ReadFileOk FileData
    | ReadFileError String
    | Reload
    | Save
    | SaveOk
    | SaveError String
    | CreateTable Bool
    | RemoveTable Int
      -- index
    | RemoveStep Int Int
      -- table index, step index
    | AddStep Bool Int Int
      -- add before, table index, step index
    | UpdateTableName Int String
      -- index, new name
    | UpdateTableField Int Int Bool String
    -- tableIndex, step index, is fixed ?, new val
    | RunTable Int
    | Tick Time
    | BackToHome
    | StartStopTable
    | StartTable Time



-- index


type alias SerializedData =
    { tables : List TableDef
    }
