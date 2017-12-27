module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (combine)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type Coordinate
    = Point Float Float
    | Arc Float Float Float


type alias Model =
    { coordinates : List Coordinate
    }


model : Model
model =
    Model []



-- UPDATE


type Msg
    = CoordinateString String


update : Msg -> Model -> Model
update msg model =
    case msg of
        CoordinateString string ->
            let
                coordinates =
                    case parseInput string of
                        Just parsed ->
                            parsed

                        Nothing ->
                            []
            in
                { model | coordinates = coordinates }


parseInput : String -> Maybe (List Coordinate)
parseInput expr =
    let
        parts =
            List.map (String.split ",")
                (String.split ";" expr)
    in
        combine (List.map parseCoordinate parts)


parseCoordinate : List String -> Maybe Coordinate
parseCoordinate input =
    case input of
        [ x, y ] ->
            case ( String.toFloat x, String.toFloat y ) of
                ( Ok x, Ok y ) ->
                    Just (Point x y)

                _ ->
                    Nothing

        [ x, y, a ] ->
            case ( String.toFloat x, String.toFloat y, String.toFloat a ) of
                ( Ok x, Ok y, Ok a ) ->
                    Just (Arc x y a)

                _ ->
                    Nothing

        _ ->
            Nothing



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "Enter a FontoBene path expression!" ]
        , input [ type_ "text", placeholder "Coordinates", onInput CoordinateString ] []
        , br [] []
        , p [] [ text (toString model.coordinates) ]
        ]
