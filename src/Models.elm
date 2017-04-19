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
    , tables : List TableDef
    , dirty : Bool
    , counter : Int
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
    , counter = 2
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
    | RemoveTable Int -- index
    | RemoveStep String Int -- table name, step index
    | AddStep Bool String Int -- add before, table name, step index


type alias SerializedData =
    { tables : List TableDef
    }