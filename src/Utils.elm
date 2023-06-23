module Utils exposing (checkForWinner, dropPiece, emptyBoard, playerToString)

import List.Extra as List
import Maybe.Extra as Maybe
import Types exposing (..)


boardWidth : Int
boardWidth =
    7


boardHeight : Int
boardHeight =
    6


emptyBoard : Board
emptyBoard =
    List.repeat boardWidth (List.repeat boardHeight Empty)


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


getRow : Board -> Int -> Maybe (List Cell)
getRow board index =
    board
        |> List.filterMap (List.getAt index)
        |> (\result ->
                if List.length result == List.length board then
                    Just result

                else
                    Nothing
           )


checkForWinner : Int -> Board -> Maybe Player
checkForWinner xPos board =
    case List.getAt xPos board of
        Nothing ->
            -- This should never happen
            Nothing

        Just playColumn ->
            case checkLineForWinner playColumn of
                Just winner ->
                    Just winner

                Nothing ->
                    let
                        yPos =
                            playColumn
                                |> List.findIndex ((==) Empty)
                                |> Maybe.map ((-) 1)
                                |> Maybe.withDefault (List.length playColumn)
                    in
                    case getRow board yPos of
                        Just playRow ->
                            case checkLineForWinner playRow of
                                Just winner ->
                                    Just winner

                                Nothing ->
                                    Nothing

                        Nothing ->
                            -- This also should never happen
                            Nothing


playerToString : Player -> String
playerToString player =
    case player of
        P1 ->
            "Player 1"

        P2 ->
            "Player 2"
