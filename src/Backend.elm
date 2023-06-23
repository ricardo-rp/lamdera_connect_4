module Backend exposing (..)

import Connect4 exposing (checkForWinner, dropPiece, switchPlayer)
import Evergreen.V1.Types exposing (Player(..))
import Lamdera exposing (ClientId, SessionId, broadcast)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { game = Connect4.init }, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ _ msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        UserClickedRow colIndex ->
            case model.game.winner of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    let
                        { game } =
                            model

                        updatedGame =
                            case dropPiece colIndex model.game.currentPlayer model.game.board of
                                Ok newBoard ->
                                    { game
                                        | board = newBoard
                                        , currentPlayer = switchPlayer game.currentPlayer
                                        , winner = checkForWinner colIndex newBoard
                                        , error = Nothing
                                    }

                                Err error ->
                                    { game | error = Just error }
                    in
                    ( { model | game = updatedGame }, broadcast (UpdateGame updatedGame) )

        UserClickedReset ->
            ( { model | game = Connect4.init }, broadcast (UpdateGame Connect4.init) )
