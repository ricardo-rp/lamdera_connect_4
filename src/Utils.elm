module Utils exposing (..)

import List.Extra as List
import Types exposing (..)


emptyBoard : Board
emptyBoard =
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
