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


dropPieceInColumn : Player -> List Cell -> Result String (List Cell)
dropPieceInColumn player column =
    case column |> List.findIndex ((==) Empty) of
        Nothing ->
            Err "Column is full"

        Just index ->
            Ok (List.setAt index (FilledBy player) column)


checkLineForWinner : List Cell -> Maybe Player
checkLineForWinner list =
    case list of
        (FilledBy P1) :: (FilledBy P1) :: (FilledBy P1) :: (FilledBy P1) :: _ ->
            Just P1

        (FilledBy P2) :: (FilledBy P2) :: (FilledBy P2) :: (FilledBy P2) :: _ ->
            Just P2

        _ :: rest ->
            checkLineForWinner rest

        [] ->
            Nothing


checkColumns : Board -> Maybe Player
checkColumns board =
    board
        |> List.filterMap checkLineForWinner
        |> List.head


getRows : Board -> Board
getRows board =
    let
        getRow index =
            board
                |> List.filterMap (List.getAt index)
                |> (\result ->
                        if List.length result == List.length board then
                            Just result

                        else
                            Nothing
                   )
    in
    List.filterMap getRow (List.range 0 7)


checkRows : Board -> Maybe Player
checkRows board =
    board
        |> getRows
        |> List.filterMap checkLineForWinner
        |> List.head


checkBoard : Board -> Maybe Player
checkBoard board =
    case checkColumns board of
        Just player ->
            Just player

        Nothing ->
            checkRows board
