module DnD.Interop exposing (..)

import DnD.Model exposing (..)
import Json.Encode as JE
import Json.Decode as JD exposing ((:=))
import Json.Decode.Extra exposing ((|:))


type alias Value =
    JE.Value



-- ENCODE


jsonStatBlocks : List StatBlock -> Value
jsonStatBlocks blocks =
    JE.list <| List.map jsonStatBlock blocks


jsonStatBlock : StatBlock -> Value
jsonStatBlock block =
    let
        ( useHitDie, health, numDie, dieFace, bonusHealth ) =
            case block.healthBlock of
                HealthValue health ->
                    ( False, health, 0, 0, 0 )

                HealthGenerator { numDie, dieFace, bonusHealth } ->
                    ( True, 0, numDie, dieFace, bonusHealth )
    in
        JE.object
            [ ( "name", JE.string block.name )
            , ( "initiative", JE.int block.initMod )
            , ( "health", JE.int health )
            , ( "numDie", JE.int numDie )
            , ( "dieFace", JE.int dieFace )
            , ( "bonusHealth", JE.int bonusHealth )
            , ( "useHitDie", JE.bool useHitDie )
            ]


jsonPhxStatBlock : StatBlock -> Value
jsonPhxStatBlock block =
    JE.object
        [ ( "stat_block", jsonStatBlock block ) ]


encodeStatBlock : StatBlock -> String
encodeStatBlock =
    JE.encode 0 << jsonStatBlock


encodePhxStatBlock : StatBlock -> String
encodePhxStatBlock =
    JE.encode 0 << jsonPhxStatBlock



-- DECODE


syncStatBlockDecoder : JD.Decoder (Syncable StatBlock)
syncStatBlockDecoder =
    JD.succeed makeSyncStatBlock
        |: ("id" := JD.int)
        |: ("name" := JD.string)
        |: ("initiative" := JD.int)
        |: ("health" := JD.int)
        |: ("numDie" := JD.int)
        |: ("dieFace" := JD.int)
        |: ("bonusHealth" := JD.int)
        |: ("useHitDie" := JD.bool)


makeSyncStatBlock : Int -> String -> Int -> Int -> Int -> Int -> Int -> Bool -> Syncable StatBlock
makeSyncStatBlock id name initMod health numDie dieFace bonusHealth useHitDie =
    if useHitDie then
        Saved id <| StatBlock name initMod <| HealthGenerator <| HitDieBlock numDie dieFace bonusHealth
    else
        Saved id <| StatBlock name initMod <| HealthValue health


syncStatBlocksDecoder : JD.Decoder (List (Syncable StatBlock))
syncStatBlocksDecoder =
    ("data" := JD.list syncStatBlockDecoder)


syncStatBlockPhxDecoder : JD.Decoder (Syncable StatBlock)
syncStatBlockPhxDecoder =
    JD.succeed Basics.identity
        |: ("data" := syncStatBlockDecoder)
