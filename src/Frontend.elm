module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Colors exposing (blue, gray, lightBlue, lightGray, lightRed, red)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input exposing (button)
import Lamdera
import Types exposing (..)
import Url
import Utils exposing (checkForWinner, dropPiece, emptyBoard)


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
      , board = emptyBoard
      , error = Nothing
      , currentPlayer = P1
      , winner = Nothing
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        ClickedRow colIndex ->
            case model.winner of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    case dropPiece colIndex model.currentPlayer model.board of
                        Ok newBoard ->
                            ( { model
                                | board = newBoard
                                , currentPlayer = switchPlayer model.currentPlayer
                                , winner = checkForWinner colIndex newBoard
                                , error = Nothing
                              }
                            , Cmd.none
                            )

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
            (column
                [ centerX
                , centerY
                , rounded 10
                , Border.solid
                , Border.color gray
                , Border.width 2
                , clip
                ]
                [ el [ centerX, padding 20, Font.size 20 ] (text "Connect 4")
                , row [] (List.indexedMap (makeColumn model.currentPlayer) model.board)
                , el [ centerX, padding 10, Font.size 12 ] (text (Maybe.withDefault "" model.error))
                , el [ centerX, padding 10, Font.size 12 ]
                    (text
                        (case model.winner of
                            Nothing ->
                                " "

                            Just winner ->
                                "Winner: " ++ Utils.playerToString winner
                        )
                    )
                ]
            )
        ]
    }


makeColumn : Player -> Int -> List Cell -> Element FrontendMsg
makeColumn currentPlayer index boardColumn =
    let
        reversedColumn =
            List.reverse boardColumn

        hoverColor =
            case currentPlayer of
                P1 ->
                    lightRed

                P2 ->
                    lightBlue
    in
    button
        [ mouseOver [ Background.color hoverColor ] ]
        { label = column [] (List.map makeCell reversedColumn)
        , onPress = Just (ClickedRow index)
        }


makeCell : Cell -> Element msg
makeCell cell =
    let
        ( content, color ) =
            case cell of
                Empty ->
                    ( " ", gray )

                FilledBy player ->
                    case player of
                        P1 ->
                            ( "x", red )

                        P2 ->
                            ( "x", blue )
    in
    el
        [ padding 5
        , Border.color gray
        , Border.solid
        , Border.width 1
        , width (px 40)
        , height (px 40)
        , Font.color color
        ]
        (el [ centerX, centerY ] (text content))
