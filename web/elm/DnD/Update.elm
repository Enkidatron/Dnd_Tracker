module DnD.Update exposing (..)

import DnD.Model exposing (..)
import Random
import String
import Regex


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        AddEntry ->
            let
                newEntry =
                    makeEntryFromEntryInput model.entryInput (addTieBreaker model.entries)

                ( newEntries, newInput ) =
                    case newEntry of
                        Nothing ->
                            ( [], model.entryInput )

                        Just entry ->
                            ( [ entry ], blankEntryInput )
            in
                { model
                    | entries = List.sortWith compareEntry (model.entries ++ newEntries)
                    , entryInput = newInput
                }
                    ! []

        RemoveEntry entry ->
            { model
                | entries = List.filter ((/=) entry) model.entries
                , entryInput = getEntryInput entry.name entry.pos.initiative entry.health
            }
                ! []

        NextTurn ->
            let
                currentPos =
                    Maybe.withDefault (getHighestPosition model.entries) model.currentTurn

                entriesLeft =
                    List.filter (\e -> (compareTurnPosition e.pos currentPos) == GT) model.entries

                nextEntry =
                    if List.length entriesLeft > 0 then
                        List.head entriesLeft
                    else
                        List.head model.entries

                nextPos =
                    Maybe.map .pos nextEntry
            in
                { model | currentTurn = nextPos } ! []

        SetInputName text ->
            let
                newInput =
                    getInput isNotBlank text

                oldEntryInput =
                    model.entryInput

                newEntryInput =
                    { oldEntryInput | name = newInput }
            in
                { model | entryInput = newEntryInput } ! []

        SetInputInit text ->
            let
                newInput =
                    getInput String.toInt text

                oldEntryInput =
                    model.entryInput

                newEntryInput =
                    { oldEntryInput | init = newInput }
            in
                { model | entryInput = newEntryInput } ! []

        SetInputHealth text ->
            let
                newInput =
                    getInput String.toInt text

                oldEntryInput =
                    model.entryInput

                newEntryInput =
                    { oldEntryInput | health = newInput }
            in
                { model | entryInput = newEntryInput } ! []

        SetHealthAdjustInput entry text ->
            let
                updateEntry e =
                    if e == entry then
                        { e | input = getInput String.toInt text }
                    else
                        e
            in
                { model | entries = List.map updateEntry model.entries } ! []

        UpdateHealth entry direction ->
            let
                operator =
                    case direction of
                        Heal ->
                            (+)

                        Damage ->
                            (-)

                amount =
                    case entry.input of
                        ValidInput _ num ->
                            num

                        _ ->
                            0

                updateEntry e =
                    if e == entry then
                        { e | health = operator e.health amount, input = NoInput }
                    else
                        e
            in
                { model | entries = List.map updateEntry model.entries } ! []

        SetSBName text ->
            let
                newInput =
                    getInput isNotBlank text

                oldBlock =
                    model.statBlockInput

                newBlock =
                    { oldBlock | name = newInput }
            in
                { model | statBlockInput = newBlock } ! []

        SetSBInitMod text ->
            let
                newInput =
                    getInput String.toInt text

                oldBlock =
                    model.statBlockInput

                newBlock =
                    { oldBlock | initMod = newInput }
            in
                { model | statBlockInput = newBlock } ! []

        SetSBHealth text ->
            let
                newInput =
                    getInput String.toInt text

                oldBlock =
                    model.statBlockInput

                newBlock =
                    { oldBlock | health = newInput }
            in
                { model | statBlockInput = newBlock } ! []

        SetSBNumDie text ->
            let
                newInput =
                    getInput String.toInt text

                oldBlock =
                    model.statBlockInput

                newBlock =
                    { oldBlock | numDie = newInput }
            in
                { model | statBlockInput = newBlock } ! []

        SetSBDieFace text ->
            let
                newInput =
                    getInput String.toInt text

                oldBlock =
                    model.statBlockInput

                newBlock =
                    { oldBlock | dieFace = newInput }
            in
                { model | statBlockInput = newBlock } ! []

        SetSBBonusHealth text ->
            let
                newInput =
                    getInput String.toInt text

                oldBlock =
                    model.statBlockInput

                newBlock =
                    { oldBlock | bonusHealth = newInput }
            in
                { model | statBlockInput = newBlock } ! []

        SetSBUseHitDie bool ->
            let
                oldBlock =
                    model.statBlockInput

                newBlock =
                    { oldBlock | useHitDie = bool }
            in
                { model | statBlockInput = newBlock } ! []

        AddStatBlock ->
            let
                sbInput =
                    model.statBlockInput

                healthBlock =
                    if sbInput.useHitDie then
                        case ( sbInput.numDie, sbInput.dieFace, sbInput.bonusHealth ) of
                            ( ValidInput _ nd, ValidInput _ df, ValidInput _ bh ) ->
                                Just <| HealthGenerator <| HitDieBlock nd df bh

                            _ ->
                                Nothing
                    else
                        case sbInput.health of
                            ValidInput _ health ->
                                Just <| HealthValue health

                            _ ->
                                Nothing

                ( newBlocks, newInput ) =
                    case ( sbInput.name, sbInput.initMod, healthBlock ) of
                        ( ValidInput _ name, ValidInput _ init, Just hb ) ->
                            ( [ StatBlock name init hb ], blankStatBlockInput )

                        _ ->
                            ( [], sbInput )
            in
                { model
                    | statBlocks = model.statBlocks ++ newBlocks
                    , statBlockInput = newInput
                }
                    ! []

        RemoveStatBlock statBlock ->
            { model | statBlocks = List.filter ((/=) statBlock) model.statBlocks } ! []

        MakeEntryFromStatBlock statBlock ->
            let
                d20PlusX x =
                    Random.int (1 + x) (20 + x)

                rollXdY x y =
                    Random.map List.sum (Random.list x (Random.int 1 y))

                rollXdYplusZ x y z =
                    Random.map ((+) z) (rollXdY x y)

                entryConstructor ( initiative, health ) =
                    AddEntryFromStatBlock statBlock.name initiative health

                statRoller =
                    case statBlock.healthBlock of
                        HealthValue val ->
                            Random.map (\x -> ( x, val )) (d20PlusX statBlock.initMod)

                        HealthGenerator { numDie, dieFace, bonusHealth } ->
                            Random.pair (d20PlusX statBlock.initMod) (rollXdYplusZ numDie dieFace bonusHealth)

                makeEntryCmd =
                    Random.generate entryConstructor statRoller
            in
                model ! [ makeEntryCmd ]

        AddEntryFromStatBlock name initiative health ->
            let
                newEntry =
                    Entry (getUniqueName model.entries name) (addTieBreaker model.entries initiative) health NoInput
            in
                { model | entries = List.sortWith compareEntry (model.entries ++ [ newEntry ]) } ! []



-- UPDATE HELPERS


makeEntryFromEntryInput : EntryInput -> (Int -> TurnPosition) -> Maybe Entry
makeEntryFromEntryInput input getTurnPosition =
    case ( input.name, input.init, input.health ) of
        ( ValidInput _ name, ValidInput _ init, ValidInput _ health ) ->
            Just <| Entry name (getTurnPosition init) health NoInput

        _ ->
            Nothing


getInput : (String -> Result String a) -> String -> Input a
getInput validator text =
    case (validator text) of
        Ok value ->
            ValidInput text value

        Err reason ->
            InvalidInput text reason


compareEntry : Entry -> Entry -> Order
compareEntry a b =
    compareTurnPosition a.pos b.pos


getEntryInput : String -> Int -> Int -> EntryInput
getEntryInput name init health =
    let
        nameInput =
            ValidInput name name

        initInput =
            ValidInput (toString init) init

        healthInput =
            ValidInput (toString health) health
    in
        EntryInput nameInput initInput healthInput


isNotBlank : String -> Result String String
isNotBlank str =
    if not <| String.isEmpty str then
        Ok str
    else
        Err "Cannot be blank"


addTieBreaker : Entries -> Int -> TurnPosition
addTieBreaker entries init =
    let
        tiedEntries =
            List.filter ((==) init << .initiative << .pos) entries

        tiebreaker =
            Maybe.withDefault 0 (List.maximum (List.map (.pos >> .tiebreaker) tiedEntries)) + 1
    in
        TurnPosition init tiebreaker


prettySuffix : String -> Int -> String
prettySuffix text suffix =
    text
        ++ if suffix <= 1 then
            ""
           else
            (" " ++ (toString suffix))


getUniqueName : Entries -> String -> String
getUniqueName entries text =
    let
        nameRegex =
            Regex.regex <| "^" ++ text ++ "( \\d*)?$"

        matchingEntries =
            List.filter (.name >> Regex.contains nameRegex) entries

        matchingNames =
            List.sort (List.map .name matchingEntries)

        suffix =
            List.foldl
                (\name i ->
                    if name == prettySuffix text i then
                        i + 1
                    else
                        i
                )
                1
                matchingNames
    in
        prettySuffix text suffix



-- COMMANDS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
