module Main exposing (Model, Msg(..), init, main, update, view, viewConverter)

import Browser
import Html exposing (Attribute, Html, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : String
    , converted : Float
    }


init : Model
init =
    Model "" 0



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newInput ->
            case String.toFloat newInput of
                Just celsius ->
                    { model
                        | input = String.fromFloat celsius
                        , converted = celsius * 1.8 + 32
                    }

                Nothing ->
                    { model
                        | converted = 0
                    }



-- VIEW


view : Model -> Html Msg
view model =
    case String.toFloat model.input of
        Just celsius ->
            viewConverter model.input "blue" (String.fromFloat (celsius * 1.8 + 32))

        Nothing ->
            viewConverter model.input "red" "???"


viewConverter : String -> String -> String -> Html Msg
viewConverter userInput color equivalentTemp =
    span []
        [ input [ value userInput, onInput Change, style "width" "40px" ] []
        , text "°C = "
        , span [ style "color" color ] [ text equivalentTemp ]
        , text "°F"
        ]
