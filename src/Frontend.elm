module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Colors exposing (blue, gray, red)
import Connect4 exposing (Cell(..), Player(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font
import Element.Input exposing (button)
import Lamdera
import Types exposing (..)
import Url
import Utils exposing (playerLightColor)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , game = Connect4.init
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

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        ClickedRow colIndex ->
            ( model, Lamdera.sendToBackend (UserClickedRow colIndex) )

        Reset ->
            ( model, Lamdera.sendToBackend UserClickedReset )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        UpdateGame game ->
            ( { model | game = game }, Cmd.none )


edges : { top : number, bottom : number, left : number, right : number }
edges =
    { top = 0, bottom = 0, left = 0, right = 0 }


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
                , paddingEach { edges | bottom = 20 }
                ]
                [ el [ centerX, padding 20, Font.size 20 ] (text "Connect 4")
                , row []
                    (List.indexedMap
                        (makeColumn model.game.currentPlayer)
                        model.game.board
                    )
                , el [ centerX, padding 10, Font.size 12 ] (text (Maybe.withDefault "" model.game.error))
                , el [ centerX, padding 10, Font.size 12 ]
                    (text
                        (case model.game.winner of
                            Nothing ->
                                " "

                            Just winner ->
                                "Winner: " ++ Connect4.playerToString winner
                        )
                    )
                , button
                    [ centerX
                    , padding 10
                    , Background.color
                        (playerLightColor model.game.currentPlayer)
                    ]
                    { label = text "Reset", onPress = Just Reset }
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
            playerLightColor currentPlayer
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
