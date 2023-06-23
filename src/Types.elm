module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Connect4
import Evergreen.V1.Types exposing (FrontendMsg(..))
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , game : Connect4.Model
    }


type alias BackendModel =
    { game : Connect4.Model }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | ClickedRow Int
    | Reset


type ToBackend
    = NoOpToBackend
    | UserClickedRow Int
    | UserClickedReset


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | UpdateGame Connect4.Model
