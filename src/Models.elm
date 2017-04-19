module Models exposing (..)


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


type alias Model =
    { state : ModelState
    , fileId : String
    , authState : AuthState
    , o2Tables : List O2TableDef
    , co2Tables : List Co2TableDef
    , dirty : Bool
    }

type alias Co2TableDef =
    { name : String
    , hold : Int
    , breaths : List Int
    }


type alias O2TableDef =
    { name : String
    , holds : List Int
    , breath : Int
    }


initialModel : Model
initialModel =
    { state = Fresh
    , fileId = ""
    , authState = AuthUnknown
    , o2Tables = []
    , co2Tables = []
    , dirty = False
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
    | RemoveStepO2 String Int -- table name, step index
    | AddStepO2 Bool String Int -- add before, table name, step index
