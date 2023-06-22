module Frontend exposing (..)

import Board exposing (Cell(..), Player(..))
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Events exposing (onClick)
import Element.Input exposing (button)
import Lamdera
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , board = Board.init
      , error = Nothing
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        ClickedRow colIndex ->
            let
                boardResult =
                    Board.dropPiece colIndex P1 model.board
            in
            case boardResult of
                Ok newBoard ->
                    ( { model | board = newBoard }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ layout []
            (column [ centerX, centerY ]
                [ text "Conect 4"
                , row [] (List.indexedMap makeColumn model.board)
                ]
            )
        ]
    }


makeColumn : Int -> Board.Column -> Element FrontendMsg
makeColumn index boardColumn =
    let
        reversedColumn =
            List.reverse boardColumn
    in
    button []
        { label = column [] (List.map makeCell reversedColumn)
        , onPress = Just (ClickedRow index)
        }


makeCell : Cell -> Element msg
makeCell cell =
    case cell of
        Empty ->
            text "E"

        FilledBy player ->
            case player of
                P1 ->
                    text "1"

                P2 ->
                    text "2"
