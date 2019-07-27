module Main exposing (Model, Msg(..), init, main, update, view, viewInput, viewValidation)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , age : String
    , password : String
    , passwordAgain : String
    , submited : Bool
    }


init : Model
init =
    Model "" "" "" "" False



-- UPDATE


type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Age age ->
            { model | age = age }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        Submit ->
            { model | submited = True }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "text" "Age" model.age Age
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , button [ onClick Submit ] [ text "Submit" ]
        , if model.submited then
            viewValidation model

          else
            div [] []
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if String.toInt model.age == Nothing then
        div [ style "color" "red" ] [ text "Age should be Number" ]

    else if String.length model.password < 3 then
        div [ style "color" "red" ] [ text "minimum 3 characters" ]

    else if String.all isNotUppercase model.password then
        div [ style "color" "red" ] [ text "chka mecatar" ]

    else if String.all isNotLowercase model.password then
        div [ style "color" "red" ] [ text "chka POQRATAR" ]

    else if String.all isNotNumeric model.password then
        div [ style "color" "red" ] [ text "chka TIV" ]

    else if model.password == model.passwordAgain then
        div [ style "color" "green" ] [ text "OK" ]

    else
        div [ style "color" "red" ] [ text "Passwords do not match!" ]


isNotUppercase : Char -> Bool
isNotUppercase character =
    List.length
        (List.filter (\x -> x == String.fromChar character) characters.alphabetUppercase)
        == 0


isNotLowercase : Char -> Bool
isNotLowercase character =
    List.length
        (List.filter (\x -> x == String.fromChar character) characters.alphabet)
        == 0


isNotNumeric : Char -> Bool
isNotNumeric character =
    List.length
        (List.filter (\x -> x == String.fromChar character) characters.numbers)
        == 0


characters =
    { alphabet = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
    , alphabetUppercase = List.map String.toUpper [ "a", "b", "c", "d", "e", "f", "g", "h" ]
    , numbers = [ "1", "2", "3" ]
    , special = [ "@", "#" ]
    }
