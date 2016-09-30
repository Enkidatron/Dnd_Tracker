module Main exposing (..)

import Html.App as App
import DnD.Model exposing (..)
import DnD.Update exposing (..)
import DnD.View exposing (..)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
