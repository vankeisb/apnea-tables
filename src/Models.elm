module Models exposing (..)

import Material
import Time exposing (Time)


type alias FileData =
    { fileId : String
    , content : String
    }


type ModelState
    = Fresh
    | Ready
    | Loading
    | Loaded
    | LoadError String
    | Saving
    | SavingError String


type AuthState
    = Authenticated
    | NotAuthenticated
    | AuthSystemFailed
    | AuthUnknown


type alias RunData =
    { table : TableDef
    , curTime : Time
    , startTime : Maybe Time
    , curStepIndex : Int
    , curStepHold : Bool
    , curStepPercent : Int
    , stopTime : Maybe Time
    , completed : Bool
    }


type alias Model =
    { state : ModelState
    , fileId : String
    , authState : AuthState
    , tables : List TableDef
    , dirty : Bool
    , runData : Maybe RunData
    , mdl : Material.Model
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
    , mdl = Material.model
    }


initRunData : TableDef -> RunData
initRunData table =
    { table = table
    , curTime = 0.0
    , startTime = Nothing
    , curStepIndex = -1
    , curStepHold = False
    , curStepPercent = 0
    , stopTime = Nothing
    , completed = False
    }


type Msg
    = AuthReady Bool
    | AuthSystemFailure
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
    | StartStopClicked
    | StartTable Time
    | Mdl (Material.Msg Msg)


isStoppedOrCompleted : RunData -> Bool
isStoppedOrCompleted rd =
    rd.completed || rd.stopTime /= Nothing


isStarted : RunData -> Bool
isStarted rd =
    rd.startTime /= Nothing


needsTick : RunData -> Bool
needsTick rd =
    isStarted rd && not (isStoppedOrCompleted rd)


totalDuration : TableDef -> Time
totalDuration t =
    let
        adder x y =
            x + y + t.fixed

        reversedSteps =
            List.reverse t.steps

        lastStep =
            List.head reversedSteps
                |> Maybe.withDefault 0

        firstSteps =
            reversedSteps
                |> List.drop 1
                |> List.reverse

        firstStepsTotal =
            firstSteps
                |> List.foldl adder 0

        lastStepTime =
            if t.isO2 then
                lastStep
            else
                t.fixed
    in
        toFloat <| (firstStepsTotal + lastStepTime) * 1000


type alias SerializedData =
    { tables : List TableDef
    }


formatTimeInterval : Time -> String
formatTimeInterval duration =
    let
        secs =
            duration / 1000

        mm =
            floor (secs / 60)
                |> toString
                |> String.padLeft 2 '0'

        ss =
            (floor secs)
                % 60
                |> toString
                |> String.padLeft 2 '0'
    in
        mm ++ ":" ++ ss


formatSeconds : Int -> String
formatSeconds secs =
    formatTimeInterval <| toFloat (secs * 1000)


stepData : RunData -> Time -> (Int, Bool, Int, Bool)
stepData runData elapsed =
    computeStepData_ 0 runData.table.steps 0 runData elapsed


computeStepData_ : Int -> List Int -> Int -> RunData -> Time -> (Int, Bool, Int, Bool)
computeStepData_ index steps total runData elapsed =
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
                    computeStepData_ (index + 1) rest (total + stepLen) runData elapsed

        [] ->
            ( -1, True, 0, False )