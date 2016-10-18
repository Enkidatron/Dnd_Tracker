module DnD.View exposing (..)

import DnD.Model exposing (..)
import Html exposing (..)
import Json.Decode as JD exposing (Decoder, (:=))
import Material
import Material.Button as Button
import Material.Scheme
import Material.Layout as Layout
import Material.Color as Color
import Material.Table as Table
import Material.Options as Options
import Material.Chip as Chip
import Material.Textfield as Textfield
import Material.Icon as Icon
import Material.Elevation as Elevation
import Material.Grid as Grid exposing (Device(..))
import Material.Toggles as Toggles


view : Model -> Html Msg
view model =
    Material.Scheme.topWithScheme Color.Teal Color.Amber <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            , Layout.selectedTab model.selectedTab
            , Layout.onSelectTab SelectTab
            ]
            { header = [ Options.div [ Options.center ] [ h4 [] [ text "D&D Tracker" ] ] ]
            , drawer = []
            , tabs = ( [ text "Encounter", text "Stat Blocks" ], [] )
            , main = [ viewBody model ]
            }


viewBody : Model -> Html Msg
viewBody model =
    case model.selectedTab of
        0 ->
            viewEncounter model

        1 ->
            viewStatBlocks model

        _ ->
            text "404"


viewEncounter : Model -> Html Msg
viewEncounter model =
    let
        currentPos =
            Maybe.withDefault (getHighestPosition model.entries) model.currentTurn
    in
        Grid.grid []
            [ Grid.cell [ Grid.size All 2 ]
                [ Button.render Mdl [ 0 ] model.mdl [ Button.onClick NextTurn, Button.raised, Button.colored, Button.ripple ] [ text "Next Turn" ] ]
            , Grid.cell [ Grid.size All 4 ]
                [ Chip.span [] [ Chip.content [] [ text <| toString currentPos ] ] ]
            , Grid.cell [ Grid.size All 12 ]
                [ Table.table []
                    [ Table.thead []
                        [ viewEntryInputRow model.mdl model.entryInput ]
                    , Table.tbody []
                        (List.indexedMap (viewEntry model.mdl) (getEntriesInTurnOrder model.entries currentPos))
                    ]
                ]
            ]


viewEntryInputRow : Material.Model -> EntryInput -> Html Msg
viewEntryInputRow mdl input' =
    Table.tr []
        [ Table.th []
            [ Textfield.render Mdl
                [ 1, 0 ]
                mdl
                [ Textfield.onInput (SetEntry EntryName >> EntryInputMsg)
                , Textfield.label "Name"
                , Textfield.floatingLabel
                , Textfield.value <| getInputString input'.name
                , showError input'.name
                ]
            ]
        , Table.th []
            [ Textfield.render Mdl
                [ 1, 1 ]
                mdl
                [ Textfield.onInput (SetEntry EntryInit >> EntryInputMsg)
                , Textfield.on "keyup" (entryKeyUpHandler EntryInit)
                , Textfield.label "Initiative"
                , Textfield.floatingLabel
                , Textfield.value <| getInputString input'.init
                , showError input'.init
                ]
            ]
        , Table.th []
            [ Textfield.render Mdl
                [ 1, 2 ]
                mdl
                [ Textfield.onInput (SetEntry EntryHealth >> EntryInputMsg)
                , Textfield.on "keyup" (entryKeyUpHandler EntryHealth)
                , Textfield.label "Health"
                , Textfield.floatingLabel
                , Textfield.value <| getInputString input'.health
                , showError input'.health
                ]
            ]
        , Table.th [] []
        , Table.th []
            [ Button.render Mdl
                [ 1, 3 ]
                mdl
                [ Button.onClick AddEntry, Button.fab, Button.ripple, disableEntryButton input' ]
                [ Icon.i "add" ]
            , Button.render Mdl [ 1, 4 ] mdl [ Button.onClick (Clear |> EntryInputMsg), Button.minifab, Button.ripple ] [ Icon.i "delete" ]
            ]
        ]


viewEntry : Material.Model -> Int -> Entry -> Html Msg
viewEntry mdl key entry =
    Table.tr [ Table.selected `Options.when` (key == 0) ]
        [ Table.td []
            [ Chip.span []
                [ Chip.contact Html.span [ Color.background Color.primary, Color.text Color.white ] [ text <| toString entry.pos.initiative ]
                , Chip.content [] [ text entry.name ]
                ]
            ]
        , Table.td [] [ Options.div [ Elevation.e4, Color.background <| getHealthColor entry.health, Color.text Color.white, Options.center ] [ text <| toString entry.health ] ]
        , Table.td []
            [ Textfield.render Mdl
                [ 2, 0, key ]
                mdl
                [ Textfield.onInput <| (SetHealthAdjust entry >> EntryMsg)
                , Textfield.on "keyup" (dynamicKeyUpHandler entry)
                , Textfield.value <| getInputString entry.input
                , showError entry.input
                ]
            ]
        , Table.td []
            [ Button.render Mdl [ 2, 1, key, 1 ] mdl [ Button.onClick <| (UpdateHealth entry Damage |> EntryMsg), Button.icon, Button.colored ] [ Icon.i "remove_circle" ]
            , Button.render Mdl [ 2, 1, key, 2 ] mdl [ Button.onClick <| (UpdateHealth entry Heal |> EntryMsg), Button.icon, Button.colored ] [ Icon.i "add_circle" ]
            ]
        , Table.td []
            [ Button.render Mdl [ 2, 2, key ] mdl [ Button.onClick <| (RemoveEntry entry |> EntryMsg), Button.minifab, Button.colored, Color.text dangerColor ] [ Icon.i "delete" ] ]
        ]


viewStatBlocks : Model -> Html Msg
viewStatBlocks model =
    Grid.grid []
        [ Grid.cell [ Grid.size All 12 ]
            [ Table.table []
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th [] [ text "Name" ]
                        , Table.th [] [ text "Init Mod" ]
                        , Table.th [] [ text "Health" ]
                        , Table.th [] [ text "" ]
                        ]
                    ]
                , Table.tbody []
                    (List.indexedMap (viewStatBlock model.mdl) model.statBlocks)
                ]
            ]
        , Grid.cell [ Grid.size All 12 ] [ viewStatBlockInput model.mdl model.statBlockInput ]
        ]


viewStatBlock : Material.Model -> Int -> Syncable StatBlock -> Html Msg
viewStatBlock mdl key syncBlock =
    let
        ( syncIcon, block ) =
            case syncBlock of
                New block ->
                    ( Icon.i "cloud_queue", block )

                Edited _ block ->
                    ( Icon.i "cloud_upload", block )

                Saved _ block ->
                    ( Icon.i "cloud_done", block )
    in
        Table.tr []
            [ Table.td [] [ text block.name ]
            , Table.td [] [ text <| formatModifier block.initMod ]
            , Table.td [] [ viewStatBlockHealthBlock mdl block.healthBlock ]
            , Table.td []
                [ Button.render Mdl [ 3, key, 0 ] mdl [ Button.raised, Button.onClick <| MakeEntryFromStatBlock block, Button.ripple, Button.colored ] [ Icon.i "add" ]
                , Button.render Mdl [ 3, key, 1 ] mdl [ Button.minifab, Button.onClick <| SaveStatBlock syncBlock, Button.ripple ] [ syncIcon ]
                , Button.render Mdl [ 3, key, 2 ] mdl [ Button.minifab, Button.onClick <| RemoveStatBlock syncBlock, Button.ripple, Button.colored, Color.text dangerColor ] [ Icon.i "delete" ]
                ]
            ]


viewStatBlockHealthBlock : Material.Model -> HealthBlock -> Html Msg
viewStatBlockHealthBlock mdl healthBlock =
    case healthBlock of
        HealthValue value ->
            text <| toString value

        HealthGenerator gen ->
            text <| (toString gen.numDie) ++ "d" ++ (toString gen.dieFace) ++ " + " ++ (toString gen.bonusHealth)


viewStatBlockInput : Material.Model -> StatBlockInput -> Html Msg
viewStatBlockInput mdl input =
    Grid.grid [ Elevation.e2 ]
        ([ Grid.cell [ Grid.size Desktop 2, Grid.size Tablet 3 ]
            [ Textfield.render Mdl
                [ 4, 0 ]
                mdl
                [ Textfield.onInput (SetSB SBName >> SBInputMsg)
                , Textfield.value <| getInputString input.name
                , Textfield.label "Name"
                , Textfield.floatingLabel
                , showError input.name
                ]
            ]
         , Grid.cell [ Grid.size Desktop 2, Grid.size Tablet 3 ]
            [ Textfield.render Mdl
                [ 4, 1 ]
                mdl
                [ Textfield.onInput (SetSB SBInitMod >> SBInputMsg)
                , Textfield.on "keyup" (sbKeyUpHandler SBInitMod)
                , Textfield.value <| getInputString input.initMod
                , Textfield.label "Initiative"
                , Textfield.floatingLabel
                , showError input.initMod
                ]
            ]
         , Grid.cell [ Grid.size All 2, Grid.size Phone 4 ]
            [ Toggles.switch Mdl
                [ 4, 2 ]
                mdl
                [ Toggles.onClick <| (ToggleUseHitDie |> SBInputMsg), Toggles.value input.useHitDie ]
                [ text "Use Hit Die" ]
            ]
         ]
            ++ (viewStatBlockInputHealthBlock mdl input)
            ++ [ Grid.cell [ Grid.size All 1 ]
                    [ Button.render Mdl [ 4, 4 ] mdl [ Button.onClick AddStatBlock, Button.fab, Button.disabled `Options.when` (not <| statBlockInputValid input) ] [ Icon.i "add" ] ]
               ]
        )


viewStatBlockInputHealthBlock : Material.Model -> StatBlockInput -> List (Grid.Cell Msg)
viewStatBlockInputHealthBlock mdl input =
    if input.useHitDie then
        [ Grid.cell [ Grid.size All 1, Grid.size Phone 3 ]
            [ Textfield.render Mdl
                [ 4, 3, 0 ]
                mdl
                [ Textfield.onInput (SetSB SBNumDie >> SBInputMsg)
                , Textfield.on "keyup" (sbKeyUpHandler SBNumDie)
                , Textfield.value <| getInputString input.numDie
                , showError input.numDie
                ]
            ]
        , Grid.cell [ Grid.size All 1 ] [ Chip.span [ Options.center ] [ Chip.content [] [ text "d" ] ] ]
        , Grid.cell [ Grid.size All 1, Grid.size Phone 3 ]
            [ Textfield.render Mdl
                [ 4, 3, 1 ]
                mdl
                [ Textfield.onInput (SetSB SBDieFace >> SBInputMsg)
                , Textfield.on "keyup" (sbKeyUpHandler SBDieFace)
                , Textfield.value <| getInputString input.dieFace
                , showError input.dieFace
                ]
            ]
        , Grid.cell [ Grid.size All 1 ] [ Chip.span [ Options.center ] [ Chip.content [] [ text "+" ] ] ]
        , Grid.cell [ Grid.size All 1, Grid.size Phone 4 ]
            [ Textfield.render Mdl
                [ 4, 3, 2 ]
                mdl
                [ Textfield.onInput (SetSB SBBonusHealth >> SBInputMsg)
                , Textfield.on "keyup" (sbKeyUpHandler SBBonusHealth)
                , Textfield.value <| getInputString input.bonusHealth
                , showError input.bonusHealth
                ]
            ]
        ]
    else
        [ Grid.cell [ Grid.size All 5 ]
            [ Textfield.render Mdl
                [ 4, 3 ]
                mdl
                [ Textfield.onInput (SetSB SBHealth >> SBInputMsg)
                , Textfield.on "keyup" (sbKeyUpHandler SBHealth)
                , Textfield.value <| getInputString input.health
                , Textfield.label "Health"
                , Textfield.floatingLabel
                ]
            ]
        ]



-- VIEW HELPERS


getEntriesInTurnOrder : Entries -> TurnPosition -> Entries
getEntriesInTurnOrder entries pos =
    let
        ( normal, wrapped ) =
            List.partition (.pos >> compareTurnPosition pos >> (/=) GT) entries
    in
        normal ++ wrapped


getInputString : Input a -> String
getInputString input =
    case input of
        NoInput ->
            ""

        InvalidInput str _ ->
            str

        ValidInput str _ ->
            str


formatModifier : Int -> String
formatModifier int =
    if int >= 0 then
        "+" ++ (toString int)
    else
        toString int


showError : Input a -> Textfield.Property b
showError input =
    case input of
        InvalidInput _ err ->
            Textfield.error err

        _ ->
            Options.nop


disableEntryButton : EntryInput -> Button.Property a
disableEntryButton entryInput =
    if entryInputValid entryInput then
        Options.nop
    else
        Button.disabled


entryInputValid : EntryInput -> Bool
entryInputValid input =
    case ( input.name, input.init, input.health ) of
        ( ValidInput _ _, ValidInput _ _, ValidInput _ _ ) ->
            True

        _ ->
            False


statBlockInputValid : StatBlockInput -> Bool
statBlockInputValid input =
    case ( input.name, input.initMod, input.useHitDie, input.health, input.numDie, input.dieFace, input.bonusHealth ) of
        ( ValidInput _ _, ValidInput _ _, True, _, ValidInput _ _, ValidInput _ _, ValidInput _ _ ) ->
            True

        ( ValidInput _ _, ValidInput _ _, False, ValidInput _ _, _, _, _ ) ->
            True

        _ ->
            False


getHealthColor : Int -> Color.Color
getHealthColor health =
    if health > 0 then
        Color.primary
    else
        dangerColor


dangerColor : Color.Color
dangerColor =
    (Color.color Color.Red Color.A700)


sbKeyUpHandler : SBInputField -> Decoder Msg
sbKeyUpHandler field =
    ("keyCode" := JD.int) `JD.andThen` (sbInterpretKeyUp field)


sbInterpretKeyUp : SBInputField -> Int -> Decoder Msg
sbInterpretKeyUp field int =
    case int of
        38 ->
            JD.succeed <| SBInputMsg <| IncrementSB field

        40 ->
            JD.succeed <| SBInputMsg <| DecrementSB field

        _ ->
            JD.fail "Not interested in this input"


entryKeyUpHandler : EntryInputField -> Decoder Msg
entryKeyUpHandler field =
    ("keyCode" := JD.int) `JD.andThen` (entryInterpretKeyUp field)


entryInterpretKeyUp : EntryInputField -> Int -> Decoder Msg
entryInterpretKeyUp field int =
    case int of
        38 ->
            JD.succeed <| EntryInputMsg <| IncrementEntry field

        40 ->
            JD.succeed <| EntryInputMsg <| DecrementEntry field

        _ ->
            JD.fail "Not interested in this input"


dynamicKeyUpHandler : Entry -> Decoder Msg
dynamicKeyUpHandler entry =
    ("keyCode" := JD.int) `JD.andThen` (dynamicInterpretKeyUp entry)


dynamicInterpretKeyUp : Entry -> Int -> Decoder Msg
dynamicInterpretKeyUp entry int =
    case int of
        38 ->
            JD.succeed <| EntryMsg <| IncrementHealthAdjust entry

        40 ->
            JD.succeed <| EntryMsg <| DecrementHealthAdjust entry

        _ ->
            JD.fail "Not interested in this input"
