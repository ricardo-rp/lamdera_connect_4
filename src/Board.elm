module Board exposing (Board, Cell(..), Column, Player(..), dropPiece, init, switchPlayer)

import List.Extra as List


type Cell
    = Empty
    | FilledBy Player


type Player
    = P1
    | P2


switchPlayer : Player -> Player
switchPlayer player =
    case player of
        P1 ->
            P2

        P2 ->
            P1


type alias Column =
    List Cell


type alias Board =
    List Column


init : Board
init =
    List.repeat 7 (List.repeat 6 Empty)


dropPiece : Int -> Player -> Board -> Result String Board
dropPiece colIndex player board =
    case List.getAt colIndex board of
        Nothing ->
            Err "Column does not exist"

        Just pickedColumn ->
            case dropPieceInColumn player pickedColumn of
                Err message ->
                    Err message

                Ok alteredColumn ->
                    Ok (List.setAt colIndex alteredColumn board)


dropPieceInColumn : Player -> Column -> Result String Column
dropPieceInColumn player column =
    case column |> List.findIndex ((==) Empty) of
        Nothing ->
            Err "Column is full"

        Just index ->
            Ok (List.setAt index (FilledBy player) column)
