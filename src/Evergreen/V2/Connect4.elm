module Evergreen.V2.Connect4 exposing (..)


type Player
    = P1
    | P2


type Cell
    = Empty
    | FilledBy Player


type alias Board =
    List (List Cell)


type alias Model =
    { board : Board
    , error : Maybe String
    , currentPlayer : Player
    , winner : Maybe Player
    }
