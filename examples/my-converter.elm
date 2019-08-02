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
    { convertible : String
    , converted : Maybe Float
    }


init : Model
init =
    Model "" Nothing



-- UPDATE


type Msg
    = Temperature String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Temperature convertible ->
            { model
                | convertible = convertible
                , converted = temperatureConverter (String.toFloat convertible)
            }


temperatureConverter : Maybe Float -> Maybe Float
temperatureConverter convertible =
    case convertible of
        Just celsius ->
            Just (celsius * 1.8 + 32)

        Nothing ->
            Nothing



-- VIEW


view : Model -> Html Msg
view model =
    span []
        [ input
            [ value model.convertible
            , onInput Temperature
            , style "border-color" (drawConvertible model.converted)
            ]
            []
        , text "°C = "
        , span [] [ text (toString model.converted) ]
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
