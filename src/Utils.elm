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

        Just column ->
            case checkLineForWinner column of
                Just winner ->
                    Just winner

                Nothing ->
                    let
                        yPos =
                            getLastPiecePosition column
                    in
                    case getRow board yPos of
                        Nothing ->
                            -- This also should never happen
                            Nothing

                        Just row ->
                            case checkLineForWinner row of
                                Just winner ->
                                    Just winner

                                Nothing ->
                                    let
                                        diagonal =
                                            getDiagonal ( xPos, yPos ) board
                                    in
                                    case checkLineForWinner diagonal of
                                        Just winner ->
                                            Just winner

                                        Nothing ->
                                            let
                                                inverseDiagonal =
                                                    getDiagonal ( xPos, yPos ) (List.reverse board)
                                            in
                                            checkLineForWinner inverseDiagonal


playerToString : Player -> String
playerToString player =
    case player of
        P1 ->
            "Player 1"

        P2 ->
            "Player 2"


getAt : ( Int, Int ) -> Board -> Maybe Cell
getAt ( x, y ) board =
    board
        |> List.getAt x
        |> Maybe.andThen (List.getAt y)


getDiagonal : ( Int, Int ) -> Board -> List Cell
getDiagonal coords board =
    let
        fromTop =
            List.reverse
                (getUpLeftDiagonal [] coords board)

        toBottom =
            getDownRightDiagonal [] coords board
                |> List.tail
                |> Maybe.withDefault []
    in
    fromTop ++ toBottom


getDownRightDiagonal : List Cell -> ( Int, Int ) -> Board -> List Cell
getDownRightDiagonal acc ( x, y ) board =
    case getAt ( x, y ) board of
        Nothing ->
            acc

        Just cell ->
            cell :: getDownRightDiagonal acc ( x + 1, y + 1 ) board


getUpLeftDiagonal : List Cell -> ( Int, Int ) -> Board -> List Cell
getUpLeftDiagonal acc ( x, y ) board =
    case getAt ( x, y ) board of
        Nothing ->
            acc

        Just cell ->
            cell :: getUpLeftDiagonal acc ( x - 1, y - 1 ) board


stringifyLine : List Cell -> String
stringifyLine list =
    let
        cellToString cell =
            case cell of
                Empty ->
                    "E"

                FilledBy player ->
                    case player of
                        P1 ->
                            "1"

                        P2 ->
                            "2"
    in
    list
        |> List.map cellToString
        |> String.join ","


getLastPiecePosition : List Cell -> Int
getLastPiecePosition list =
    let
        firstEmtpyIndex =
            List.findIndex ((==) Empty) list
    in
    case firstEmtpyIndex of
        Nothing ->
            List.length list

        Just index ->
            index - 1
