module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , status : Status
    }


type Status
    = Move
    | Stopping


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) Move
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | Stop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        Stop ->
            case model.status of
                Move ->
                    ( { model | status = Stopping }, Cmd.none )

                Stopping ->
                    ( { model | status = Move }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Move ->
            Time.every 1000 Tick

        Stopping ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour model.zone model.time)

        minute =
            String.fromInt (Time.toMinute model.zone model.time)

        second =
            String.fromInt (Time.toSecond model.zone model.time)

        pi =
            3.141592

        angle =
            -- turns : Float -> Float(Convert turns to standard Elm angles(radians))
            -- ex) turns 180 / pi  --=> 360 : Float, turnsは2πのこと
            -- Time.inMinutes model で現在UNIX時刻を分表示する
            turns <| toFloat <| Time.toSecond model.zone model.time

        handX =
            String.fromFloat (50.0 + 40.0 * pi * angle / 180)

        handY =
            String.fromFloat (50.0 + 40.0 * pi * angle / 180)
    in
    div []
        [ h1 [] [ Html.text (hour ++ ":" ++ minute ++ ":" ++ second) ]
        , h1 [] [ Html.text (String.fromFloat angle) ]
        , h1 [] [ Html.text handX ]
        , h1 [] [ Html.text handY ]
        , button [ onClick Stop ] [ Html.text "move or stop" ]
        , svg
            [ width "120"
            , height "120"
            , viewBox "0 0 120 120"
            ]
            [ circle
                [ cx "50"
                , cy "50"
                , r "50"
                ]
                []
            , line
                [ x1 "50"
                , y1 "50"
                , x2 handX
                , y2 handY
                , stroke "yellow"
                , strokeWidth "5"
                , strokeMiterlimit "10"
                , fill "True"
                ]
                []
            ]
        ]
