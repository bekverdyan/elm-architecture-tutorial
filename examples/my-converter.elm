module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { inputTemp : String
    , convertedTemp : Maybe Float
    , inputHeft : String
    , convertedHeft : Maybe Float
    }


init : Model
init =
    Model "" Nothing "" Nothing



-- UPDATE


type Msg
    = TemperatureInput String
    | HeftInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        TemperatureInput convertible ->
            { model
                | inputTemp = convertible
                , convertedTemp = temperatureConverter (String.toFloat convertible)
            }

        HeftInput convertible ->
            { model
                | inputHeft = convertible
                , convertedHeft = heftConverter (String.toFloat convertible)
            }


temperatureConverter : Maybe Float -> Maybe Float
temperatureConverter convertible =
    case convertible of
        Just celsius ->
            Just (celsius * 1.8 + 32)

        Nothing ->
            Nothing


heftConverter : Maybe Float -> Maybe Float
heftConverter convertible =
    case convertible of
        Just kilo ->
            Just (kilo / 0.45359237)

        Nothing ->
            Nothing



-- VIEW


type alias Measurment =
    { name : String
    , convertibleSymbol : String
    , convertedSymbol : String
    }


view : Model -> Html Msg
view model =
    div []
        [ viewConverter temperature model.inputTemp model.convertedTemp TemperatureInput
        , viewConverter heft model.inputHeft model.convertedHeft HeftInput
        ]


temperature : Measurment
temperature =
    { name = "Temperature", convertibleSymbol = "°C = ", convertedSymbol = "°F" }


heft : Measurment
heft =
    { name = "Heft", convertibleSymbol = "kilo", convertedSymbol = "lbs" }


viewConverter : Measurment -> String -> Maybe Float -> (String -> Msg) -> Html Msg
viewConverter measurment convertible converted toMsg =
    div []
        [ span []
            [ input
                [ value convertible
                , onInput toMsg
                , style "border-color" (drawConvertible converted)
                ]
                []
            , text measurment.convertibleSymbol
            , span [ style "color" (drawConverted converted) ] [ text (toString converted) ]
            , text measurment.convertedSymbol
            ]
        ]


toString : Maybe Float -> String
toString converted =
    case converted of
        Just fahrenheit ->
            String.fromFloat fahrenheit

        Nothing ->
            "???"


drawConvertible : Maybe Float -> String
drawConvertible converted =
    case converted of
        Just _ ->
            ""

        Nothing ->
            "red"


drawConverted : Maybe Float -> String
drawConverted converted =
    case converted of
        Just _ ->
            "green"

        Nothing ->
            "red"
