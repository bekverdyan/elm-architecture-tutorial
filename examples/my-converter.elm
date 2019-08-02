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
    , converted : Float
    }


init : Model
init =
    Model "" 0



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


temperatureConverter : Maybe Float -> Float
temperatureConverter convertible =
    case convertible of
        Just celsius ->
            celsius * 1.8 + 32

        Nothing ->
            0



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ value model.convertible, onInput Temperature ] []
        , div [] [ text (String.fromFloat model.converted) ]
        ]
