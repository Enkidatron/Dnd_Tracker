module Main exposing (..)

import Html.App as App
import DnD.Model exposing (..)
import DnD.Update exposing (..)
import DnD.View exposing (..)


main : Program String
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : String -> ( Model, Cmd Msg )
init str =
    ( newModel str, fetchAll str )
