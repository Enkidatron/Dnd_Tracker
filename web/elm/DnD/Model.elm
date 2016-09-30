module DnD.Model exposing (..)


type Msg
    = NoOp
    | AddEntry
    | RemoveEntry Entry
    | NextTurn
    | SetInputName String
    | SetInputInit String
    | SetInputHealth String
    | SetHealthAdjustInput Entry String
    | UpdateHealth Entry HealthAdjustment
    | SetSBName String
    | SetSBInitMod String
    | SetSBHealth String
    | SetSBNumDie String
    | SetSBDieFace String
    | SetSBBonusHealth String
    | SetSBUseHitDie Bool
    | AddStatBlock
    | RemoveStatBlock StatBlock
    | MakeEntryFromStatBlock StatBlock
    | AddEntryFromStatBlock String Int Int


type alias Entry =
    { name : String
    , pos : TurnPosition
    , health : Int
    , input : Input Int
    }


type alias Entries =
    List Entry


type HealthAdjustment
    = Heal
    | Damage


type alias TurnPosition =
    { initiative : Int
    , tiebreaker : Int
    }


type alias StatBlock =
    { name : String
    , initMod : Int
    , healthBlock : HealthBlock
    }


type HealthBlock
    = HealthValue Int
    | HealthGenerator HitDieBlock


type alias HitDieBlock =
    { numDie : Int
    , dieFace : Int
    , bonusHealth : Int
    }


type Input t
    = NoInput
    | InvalidInput String String
    | ValidInput String t


type alias EntryInput =
    { name : Input String
    , init : Input Int
    , health : Input Int
    }


type alias StatBlockInput =
    { name : Input String
    , initMod : Input Int
    , health : Input Int
    , numDie : Input Int
    , dieFace : Input Int
    , bonusHealth : Input Int
    , useHitDie : Bool
    }


type alias Model =
    { entries : Entries
    , currentTurn : Maybe TurnPosition
    , entryInput : EntryInput
    , statBlocks : List StatBlock
    , statBlockInput : StatBlockInput
    }


blankEntryInput : EntryInput
blankEntryInput =
    EntryInput NoInput NoInput NoInput


blankStatBlockInput : StatBlockInput
blankStatBlockInput =
    StatBlockInput NoInput NoInput NoInput NoInput NoInput NoInput True


model : Model
model =
    Model [] Nothing blankEntryInput [] blankStatBlockInput


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- COMMON HELPERS


getHighestPosition : Entries -> TurnPosition
getHighestPosition entries =
    let
        topEntry =
            List.head entries

        maybePos =
            Maybe.map .pos topEntry
    in
        Maybe.withDefault (TurnPosition 0 0) maybePos


compareTurnPosition : TurnPosition -> TurnPosition -> Order
compareTurnPosition a b =
    case compare a.initiative b.initiative of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            compare a.tiebreaker b.tiebreaker
