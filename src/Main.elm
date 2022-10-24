module Main exposing (..)

import Browser
import Browser.Events exposing (..)
import Json.Decode as Decode
import Svg exposing (Svg, circle, rect, svg, text, text_)
import Svg.Attributes exposing (..)


main : Program () Model Msg
main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { playerOnePosY : Int
    , playerTwoPosY : Int
    , playerOneScore : Int
    , playerTwoScore : Int
    , ballPosX : Int
    , ballPosY : Int
    , ballDeltaX : Int
    , ballDeltaY : Int
    , ballDeltaXCopy : Int
    , ballDeltaYCopy : Int
    , gameMode : GameMode
    , isPaused : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model startYPos startYPos 0 0 300 100 0 0 xSpeed ySpeed SinglePlayerVsSelf True
    , Cmd.none
    )


type GameMode
    = SinglePlayerVsSelf
    | SinglePlayerVsBot
    | TwoPlayer


type Msg
    = MovePlayerOneUp
    | MovePlayerOneDown
    | MovePlayerTwoUp
    | MovePlayerTwoDown
    | MoveBall Float
    | TogglePauseGame
    | ToggleSinglePlayerBot
    | Other


moveDelta : Int
moveDelta =
    10


startYPos : Int
startYPos =
    maxYPos // 2


maxYPos : Int
maxYPos =
    400 - 45 - 10


getPosInBounds : Int -> Int
getPosInBounds posY =
    if posY >= maxYPos then
        maxYPos

    else if posY <= 10 then
        10

    else
        posY


xSpeed : Int
xSpeed =
    5


ySpeed : Int
ySpeed =
    2


getIsCollision : Model -> Int -> Int -> Bool
getIsCollision model paddlePosX paddlePosY =
    let
        ballLeft =
            model.ballPosX - ballRadius

        ballRight =
            model.ballPosX + ballRadius

        ballTop =
            model.ballPosY - ballRadius

        ballBottom =
            model.ballPosY + ballRadius

        paddleLeft =
            paddlePosX

        paddleRight =
            paddlePosX + paddleWidth

        paddleTop =
            paddlePosY

        paddleBottom =
            paddlePosY + paddleHeight

        doesBallLeftOverlap =
            ballLeft >= paddleLeft && ballLeft <= paddleRight

        doesBallRightOverlap =
            ballRight >= paddleLeft && ballRight <= paddleRight

        doesBallTopOverlap =
            ballTop >= paddleTop && ballTop <= paddleBottom

        doesBallBottomOverlap =
            ballBottom >= paddleTop && ballBottom <= paddleBottom
    in
    (doesBallLeftOverlap || doesBallRightOverlap) && (doesBallTopOverlap || doesBallBottomOverlap)


getIsOutOfBounds : Int -> Int -> Int -> ( Bool, Bool )
getIsOutOfBounds posY lowerBound upperBound =
    let
        isTooHigh =
            posY <= lowerBound

        isTooLow =
            posY >= upperBound
    in
    ( isTooHigh, isTooLow )


getNextYDelta : Model -> Int
getNextYDelta model =
    let
        ( isTooHigh, isTooLow ) =
            getIsOutOfBounds model.ballPosY 0 400
    in
    if isTooHigh then
        ySpeed

    else if isTooLow then
        -ySpeed

    else
        model.ballDeltaY


getIsOutOfBoundsX : Model -> ( Bool, Bool )
getIsOutOfBoundsX model =
    ( model.ballPosX >= 600, model.ballPosX <= 0 )


getNextXDelta : Model -> Int
getNextXDelta model =
    let
        ( isTooFarRight, isTooFarLeft ) =
            getIsOutOfBoundsX model

        isLeftPaddleCollison =
            getIsCollision model paddleOneXPos model.playerOnePosY

        isRightPaddleCollison =
            getIsCollision model paddleTwoXPos model.playerTwoPosY

        isCollision =
            isLeftPaddleCollison || isRightPaddleCollison
    in
    if isCollision then
        -model.ballDeltaX

    else if isTooFarRight then
        -xSpeed

    else if isTooFarLeft then
        xSpeed

    else
        model.ballDeltaX


getNextDeltas : Model -> ( Int, Int )
getNextDeltas model =
    ( getNextXDelta model, getNextYDelta model )


getRightPaddlePosY : Model -> Int
getRightPaddlePosY model =
    let
        isBallIncoming =
            model.ballDeltaX > 0

        isAboveMiddle =
            model.playerTwoPosY < startYPos

        isBelowMiddle =
            model.playerTwoPosY > startYPos

        ( isTooHigh, isTooLow ) =
            getIsOutOfBounds model.playerTwoPosY 10 345
    in
    if model.gameMode == SinglePlayerVsBot then
        if isBallIncoming && (isTooHigh || isTooLow) then
            model.playerTwoPosY

        else if isBallIncoming then
            model.playerTwoPosY + model.ballDeltaY

        else if isAboveMiddle then
            model.playerTwoPosY + 1

        else if isBelowMiddle then
            model.playerTwoPosY - 1

        else
            model.playerTwoPosY

    else
        model.playerTwoPosY


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePauseGame ->
            if model.isPaused then
                ( { model | isPaused = False, ballDeltaX = model.ballDeltaXCopy, ballDeltaY = model.ballDeltaYCopy }
                , Cmd.none
                )

            else
                ( { model | isPaused = True, ballDeltaX = 0, ballDeltaY = 0, ballDeltaXCopy = model.ballDeltaX, ballDeltaYCopy = model.ballDeltaY }
                , Cmd.none
                )

        ToggleSinglePlayerBot ->
            if model.gameMode == SinglePlayerVsSelf then
                ( { model | gameMode = SinglePlayerVsBot }, Cmd.none )

            else
                ( { model | gameMode = SinglePlayerVsSelf }, Cmd.none )

        MovePlayerOneUp ->
            ( { model | playerOnePosY = getPosInBounds model.playerOnePosY - moveDelta }
            , Cmd.none
            )

        MovePlayerOneDown ->
            ( { model | playerOnePosY = getPosInBounds model.playerOnePosY + moveDelta }
            , Cmd.none
            )

        MovePlayerTwoUp ->
            ( { model | playerTwoPosY = getPosInBounds model.playerTwoPosY - moveDelta }
            , Cmd.none
            )

        MovePlayerTwoDown ->
            ( { model | playerTwoPosY = getPosInBounds model.playerTwoPosY + moveDelta }
            , Cmd.none
            )

        MoveBall _ ->
            let
                ( xDelta, yDelta ) =
                    getNextDeltas model

                newBallPosX =
                    model.ballPosX + xDelta

                newBallPosY =
                    model.ballPosY + yDelta

                ( isTooFarRight, isTooFarLeft ) =
                    getIsOutOfBoundsX model

                updatedModel =
                    { model | ballPosX = newBallPosX, ballDeltaX = xDelta, ballPosY = newBallPosY, ballDeltaY = yDelta, playerTwoPosY = getRightPaddlePosY model }
            in
            if isTooFarLeft then
                ( { updatedModel | playerTwoScore = model.playerTwoScore + 1 }, Cmd.none )

            else if isTooFarRight then
                ( { updatedModel | playerOneScore = model.playerOneScore + 1 }, Cmd.none )

            else
                ( updatedModel, Cmd.none )

        Other ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            keyDecoder
        , Browser.Events.onAnimationFrameDelta
            MoveBall
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKeyboardInput (Decode.field "key" Decode.string)


toKeyboardInput : String -> Msg
toKeyboardInput key =
    case key of
        "p" ->
            TogglePauseGame

        "w" ->
            MovePlayerOneUp

        "s" ->
            MovePlayerOneDown

        "b" ->
            ToggleSinglePlayerBot

        "ArrowUp" ->
            MovePlayerTwoUp

        "ArrowDown" ->
            MovePlayerTwoDown

        _ ->
            Other


view : Model -> Browser.Document Msg
view model =
    { title = "Pong Game"
    , body = [ backdrop model ]
    }


iToS : Int -> String
iToS num =
    String.fromInt num


backdrop : Model -> Svg msg
backdrop model =
    svg
        [ viewBox "0 0 600 400"
        , width "600"
        , height "400"
        ]
        [ rect [ width "100%", height "100%" ] []
        , paddleOne model.playerOnePosY
        , paddleTwo model.playerTwoPosY
        , ball model.ballPosX model.ballPosY
        , text_ [ fill "white", x "10", y "390" ] [ text ("Player One: " ++ iToS model.playerOneScore) ]
        , text_ [ fill "white", x "490", y "390" ] [ text ("Player Two: " ++ iToS model.playerTwoScore) ]
        , button "Press p to start" 255 196
        ]


button : String -> Int -> Int -> Svg msg
button label posX posY =
    let
        textPosX =
            posX + 7

        textPosY =
            posY + 18
    in
    svg [ style "cursor: pointer" ]
        [ rect [ width "108", height "26", stroke "white", fillOpacity "0", x (iToS posX), y (iToS posY) ] []
        , text_ [ fill "white", x (iToS textPosX), y (iToS textPosY) ] [ Svg.text label ]
        ]


ballRadius : Int
ballRadius =
    12


ball : Int -> Int -> Svg msg
ball posX posY =
    circle [ fill "white", r (iToS ballRadius), cx (iToS posX), cy (iToS posY) ] []


paddleOneXPos : Int
paddleOneXPos =
    40


paddleTwoXPos : Int
paddleTwoXPos =
    540


paddleOne : Int -> Svg msg
paddleOne yPos =
    paddle "red" paddleOneXPos yPos


paddleTwo : Int -> Svg msg
paddleTwo yPos =
    paddle "skyblue" paddleTwoXPos yPos


paddleWidth : Int
paddleWidth =
    20


paddleHeight : Int
paddleHeight =
    45


paddle : String -> Int -> Int -> Svg msg
paddle color xPosInt yPosInt =
    let
        xPos =
            iToS xPosInt

        yPos =
            iToS yPosInt
    in
    rect
        [ fill color, x xPos, y yPos, width (iToS paddleWidth), height (iToS paddleHeight) ]
        []
