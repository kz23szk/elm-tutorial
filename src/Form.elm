module Main exposing (Model, Msg(..), init, main, update, view, viewInput, viewValidation)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    }


init : Model
init =
    Model "" "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain passwordAgain ->
            { model | passwordAgain = passwordAgain }

        Age age ->
            { model | age = age }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewInput "age" "age" model.age Age
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if isSameString model && isEnouphLength model && isComplexPassword model && isCorrectAge model then
        div [ style "color" "green" ] [ text "OK" ]

    else if isEnouphLength model == False then
        div [ style "color" "red" ] [ text "Password's length must over 8 !" ]

    else if isComplexPassword model == False then
        div [ style "color" "red" ] [ text "Password must contain upper and lower case letters and numbers." ]

    else if isCorrectAge model == False then
        div [ style "color" "red" ] [ text "Enter correct age." ]

    else
        div [ style "color" "red" ] [ text "Passwords do not match!" ]


isSameString : Model -> Bool
isSameString { password, passwordAgain } =
    password == passwordAgain


isEnouphLength : Model -> Bool
isEnouphLength { password } =
    if String.length password > 8 then
        True

    else
        False


isCorrectAge : Model -> Bool
isCorrectAge { age } =
    String.toInt age /= Nothing


isComplexPassword : Model -> Bool
isComplexPassword { password } =
    if List.any isNum (String.toList password) && List.any isLowerCase (String.toList password) && List.any isUpperCase (String.toList password) then
        True

    else
        False


isNum : Char -> Bool
isNum c =
    if '0' <= c && c <= '9' then
        True

    else
        False


isLowerCase : Char -> Bool
isLowerCase c =
    if 'a' <= c && c <= 'z' then
        True

    else
        False


isUpperCase : Char -> Bool
isUpperCase c =
    if 'A' <= c && c <= 'Z' then
        True

    else
        False
