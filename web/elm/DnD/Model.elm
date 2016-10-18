module DnD.Model exposing (..)

import Http
import Material


type Msg
    = NoOp
    | AddEntry
    | NextTurn
    | EntryInputMsg EntryInputMsg
    | EntryMsg EntryMsg
    | SBInputMsg SBInputMsg
    | AddStatBlock
    | MakeEntryFromStatBlock StatBlock
    | AddEntryFromStatBlock String Int Int
    | FetchAll
    | FetchAllSucceed (List (Syncable StatBlock))
    | HttpFail Http.Error
    | SaveStatBlock (Syncable StatBlock)
    | SaveStatBlockSucceed (Syncable StatBlock)
    | RemoveStatBlock (Syncable StatBlock)
    | Mdl (Material.Msg Msg)
    | SelectTab Int


type EntryInputMsg
    = SetEntry EntryInputField String
    | IncrementEntry EntryInputField
    | DecrementEntry EntryInputField
    | Clear


type EntryInputField
    = EntryName
    | EntryInit
    | EntryHealth


type SBInputMsg
    = SetSB SBInputField String
    | IncrementSB SBInputField
    | DecrementSB SBInputField
    | ToggleUseHitDie


type SBInputField
    = SBName
    | SBInitMod
    | SBHealth
    | SBNumDie
    | SBDieFace
    | SBBonusHealth


type EntryMsg
    = SetHealthAdjust Entry String
    | IncrementHealthAdjust Entry
    | DecrementHealthAdjust Entry
    | UpdateHealth Entry HealthAdjustment
    | RemoveEntry Entry


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


type Syncable a
    = New a
    | Edited Int a
    | Saved Int a


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
    , statBlocks : List (Syncable StatBlock)
    , statBlockInput : StatBlockInput
    , url : String
    , mdl : Material.Model
    , selectedTab : Int
    }


blankEntryInput : EntryInput
blankEntryInput =
    EntryInput NoInput NoInput NoInput


blankStatBlockInput : StatBlockInput
blankStatBlockInput =
    StatBlockInput NoInput NoInput NoInput NoInput NoInput NoInput True


blankModel : Model
blankModel =
    { entries = []
    , currentTurn = Nothing
    , entryInput = blankEntryInput
    , statBlocks = []
    , statBlockInput = blankStatBlockInput
    , url = ""
    , mdl = Material.model
    , selectedTab = 0
    }


newModel : String -> Model
newModel str =
    { blankModel | url = str }



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
