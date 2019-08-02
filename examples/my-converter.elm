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
    = Temperature String
    | Heft String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Temperature convertible ->
            { model
                | inputTemp = convertible
                , convertedTemp = temperatureConverter (String.toFloat convertible)
            }

        Heft convertible ->
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


view : Model -> Html Msg
view model =
    span []
        [ input
            [ value model.inputTemp
            , onInput Temperature
            , style "border-color" (drawConvertible model.convertedTemp)
            ]
            []
        , text "°C = "
        , span [ style "color" (drawConverted model.convertedTemp) ] [ text (toString model.convertedTemp) ]
        , text "°F"
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
