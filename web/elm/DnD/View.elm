module DnD.View exposing (..)

import DnD.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onCheck)
import Bootstrap.Grid exposing (..)
import Bootstrap.Buttons exposing (..)
import Bootstrap.Wells exposing (..)


view : Model -> Html Msg
view model =
    let
        currentPos =
            Maybe.withDefault (getHighestPosition model.entries) model.currentTurn
    in
        container
            [ table [ class "table table-condensced" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Initiative" ]
                        , th [] [ text "Health" ]
                        , th [] [ text "" ]
                        ]
                    ]
                ]
            , tbody []
                ([ (viewInputRow model.entryInput) ] ++ List.indexedMap viewEntry (getEntriesInTurnOrder model.entries currentPos))
            , btn BtnPrimary [] [] [ onClick NextTurn ] [ text "Next Turn" ]
            , well WellNormal [] [ text ("init: " ++ (toString currentPos.initiative) ++ ", tiebreaker: " ++ (toString currentPos.tiebreaker)) ]
            , viewStatBlockTable model.statBlockInput model.statBlocks
            , btn BtnPrimary [] [] [ onClick FetchAll ] [ text "Fetch All" ]
            ]


viewInputRow : EntryInput -> Html Msg
viewInputRow entryInput =
    let
        buttonAttrs =
            case ( entryInput.name, entryInput.init, entryInput.health ) of
                ( ValidInput _ _, ValidInput _ _, ValidInput _ _ ) ->
                    []

                _ ->
                    [ disabled True ]
    in
        tr []
            [ viewFormGroup "Name" "text" entryInput.name SetInputName
            , viewFormGroup "Initiative" "number" entryInput.init SetInputInit
            , viewFormGroup "Health" "number" entryInput.health SetInputHealth
            , td [] [ btn BtnPrimary [] [] ([ onClick AddEntry ] ++ buttonAttrs) [ text "Add" ] ]
            ]


viewFormGroup : String -> String -> Input a -> (String -> Msg) -> Html Msg
viewFormGroup name inputtype input' msg =
    let
        ( displayText, classStr ) =
            case input' of
                ValidInput raw _ ->
                    ( raw, "" )

                InvalidInput raw _ ->
                    ( raw, " has-warning" )

                NoInput ->
                    ( "", "" )
    in
        td []
            [ div [ class <| "form-group" ++ classStr ]
                [ input [ type' inputtype, class "form-control", onInput msg, placeholder name, value displayText ] []
                ]
            ]


viewEntry : Int -> Entry -> Html Msg
viewEntry index entry =
    let
        rowClass =
            if (index == 0) then
                "info"
            else
                ""

        healthDivClass =
            if (entry.health > 0) then
                "input-group has-success"
            else
                "input-group has-error"

        displayAmount =
            case entry.input of
                ValidInput raw _ ->
                    raw

                InvalidInput raw _ ->
                    raw

                NoInput ->
                    ""
    in
        tr [ class rowClass ]
            [ td [] [ text entry.name ]
            , td [] [ span [ class "badge" ] [ text (toString entry.pos.initiative) ] ]
            , td []
                [ Html.form [ class "form-inline" ]
                    [ div [ class healthDivClass ]
                        [ span [ class "input-group-addon" ] [ text (toString entry.health) ]
                        , input [ type' "number", class "form-control form-inline", placeholder "Amount", value displayAmount, onInput <| SetHealthAdjustInput entry ] []
                        , div [ class "input-group-btn" ]
                            [ btn BtnDanger [] [] [ onClick <| UpdateHealth entry Damage ] [ text "Damage" ]
                            , btn BtnInfo [] [] [ onClick <| UpdateHealth entry Heal ] [ text "Heal" ]
                            ]
                        ]
                    ]
                ]
            , td [] [ btn BtnDanger [ BtnExtraSmall ] [] [ onClick <| RemoveEntry entry ] [ text "X" ] ]
            ]


viewStatBlockTable : StatBlockInput -> List (Syncable StatBlock) -> Html Msg
viewStatBlockTable sbInput statBlocks =
    table [ class "table table-condensced" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Initiative Modifier" ]
                , th [] [ text "" ]
                , th [] [ text "Health" ]
                , th [] [ text "Add" ]
                , th [] [ text "" ]
                , th [] [ text "" ]
                ]
            ]
        , tbody []
            ([ viewStatBlockInputRow sbInput ] ++ List.map viewStatBlock statBlocks)
        ]


viewStatBlockInputRow : StatBlockInput -> Html Msg
viewStatBlockInputRow sbInput =
    tr []
        [ viewFormGroup "Name" "text" sbInput.name SetSBName
        , viewFormGroup "Initiative" "number" sbInput.initMod SetSBInitMod
        , td []
            [ Html.form []
                [ div [ class "checkbox" ]
                    [ label [] [ input [ type' "checkbox", onCheck SetSBUseHitDie, checked sbInput.useHitDie ] [], text "Use Hit Die" ] ]
                ]
            ]
        , if sbInput.useHitDie then
            (viewHitDieInput sbInput)
          else
            viewFormGroup "Health" "number" sbInput.health SetSBHealth
        , td [] [ btn BtnPrimary [] [] [ onClick AddStatBlock ] [ text "+" ] ]
        , td [] []
        , td [] []
        ]


viewHitDieInput : StatBlockInput -> Html Msg
viewHitDieInput sbInput =
    td [ style [ ( "width", "33%" ) ] ]
        [ Html.form [ class "form-inline" ]
            [ div [ class "input-group" ]
                [ input [ type' "number", class "form-control form-inline", value (getInputString sbInput.numDie), onInput SetSBNumDie ] []
                , span [ class "input-group-addon" ] [ text "d" ]
                , input [ type' "number", class "form-control form-inline", value (getInputString sbInput.dieFace), onInput SetSBDieFace ] []
                , span [ class "input-group-addon" ] [ text "+" ]
                , input [ type' "number", class "form-control form-inline", value (getInputString sbInput.bonusHealth), onInput SetSBBonusHealth ] []
                ]
            ]
        ]


viewStatBlock : Syncable StatBlock -> Html Msg
viewStatBlock syncBlock =
    let
        ( prependText, statBlock ) =
            case syncBlock of
                New block ->
                    ( "! ", block )

                Edited _ block ->
                    ( "* ", block )

                Saved _ block ->
                    ( "", block )
    in
        tr []
            [ td [] [ text <| prependText ++ statBlock.name ]
            , td []
                [ span [ class "badge" ]
                    [ text
                        ((if statBlock.initMod > 0 then
                            "+"
                          else
                            ""
                         )
                            ++ (toString statBlock.initMod)
                        )
                    ]
                ]
            , td [] []
            , td []
                [ text <|
                    case statBlock.healthBlock of
                        HealthGenerator { numDie, dieFace, bonusHealth } ->
                            ((toString numDie) ++ "d" ++ (toString dieFace) ++ " + " ++ (toString bonusHealth))

                        HealthValue val ->
                            toString val
                ]
            , td [] [ btn BtnPrimary [] [] [ onClick <| MakeEntryFromStatBlock statBlock ] [ text "^" ] ]
            , td [] [ btn BtnInfo [ BtnExtraSmall ] [] [ onClick <| SaveStatBlock syncBlock ] [ text "Save" ] ]
            , td [] [ btn BtnDanger [ BtnExtraSmall ] [] [ onClick <| RemoveStatBlock syncBlock ] [ text "X" ] ]
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
