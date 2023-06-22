module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , board : Board
    , error : Maybe String
    , currentPlayer : Player
    , winner : Maybe Player
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | ClickedRow Int


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend


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


type alias Board =
    List (List Cell)
