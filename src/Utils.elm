module Utils exposing (playerLightColor)

import Colors exposing (lightBlue, lightRed)
import Connect4 exposing (Player(..))
import Element exposing (Color)
import Types exposing (..)


playerLightColor : Player -> Color
playerLightColor player =
    case player of
        P1 ->
            lightRed

        P2 ->
            lightBlue
