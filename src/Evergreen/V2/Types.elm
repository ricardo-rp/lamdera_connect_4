module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Evergreen.V2.Connect4
import Url


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , game : Evergreen.V2.Connect4.Model
    }


type alias BackendModel =
    { game : Evergreen.V2.Connect4.Model
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
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
    | UpdateGame Evergreen.V2.Connect4.Model
