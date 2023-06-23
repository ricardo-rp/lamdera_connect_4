module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Url


type Player
    = P1
    | P2


type Cell
    = Empty
    | FilledBy Player


type alias Board =
    List (List Cell)


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , board : Board
    , error : Maybe String
    , currentPlayer : Player
    , winner : Maybe Player
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | ClickedRow Int


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
