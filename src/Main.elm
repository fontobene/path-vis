module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (combine)
import Svg exposing (Svg, svg, g, circle, line)
import Svg.Attributes exposing (cx, cy, r, x1, x2, y1, y2, height, width, stroke, strokeWidth, viewBox)


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
        , svg [ height "200", width "200", viewBox "-0.5 -0.5 10 10" ]
            [ makeGrid
            , drawPolyline model.coordinates
            , circle [ cx "20", cy "20", r "4" ] []
            ]
        ]


makeGrid : Svg Msg
makeGrid =
    g [] (makeGridHorizontal ++ makeGridVertical)


gridColor : String
gridColor =
    "#DDD"


makeGridHorizontal : List (Svg Msg)
makeGridHorizontal =
    List.map
        (\x ->
            line
                [ x1 <| toString x
                , y1 "0"
                , x2 <| toString x
                , y2 "9"
                , stroke gridColor
                , strokeWidth "0.1"
                ]
                []
        )
        (List.range 0 9)


makeGridVertical : List (Svg Msg)
makeGridVertical =
    List.map
        (\y ->
            line
                [ x1 "0"
                , y1 <| toString y
                , x2 "9"
                , y2 <| toString y
                , stroke gridColor
                , strokeWidth "0.1"
                ]
                []
        )
        (List.range 0 9)


drawPolyline : List Coordinate -> Svg Msg
drawPolyline coords =
    g [] <|
        (case List.head coords of
            Just coord ->
                [ drawPoint coord ]

            Nothing ->
                []
        )
            ++ (List.map2 drawCoordinate coords (List.drop 1 coords))


drawCoordinate : Coordinate -> Coordinate -> Svg Msg
drawCoordinate prevCoord coord =
    case coord of
        Point x y ->
            g []
                [ drawPoint coord
                , drawLine prevCoord coord
                ]

        Arc x y a ->
            g []
                [ drawPoint coord
                ]


getSvgCoords : Coordinate -> ( String, String )
getSvgCoords coord =
    case coord of
        Point x y ->
            ( toString x, toString <| 9 - y )

        Arc x y _ ->
            ( toString x, toString <| 9 - y )


drawPoint : Coordinate -> Svg Msg
drawPoint coord =
    let
        ( x, y ) =
            getSvgCoords coord
    in
        circle [ cx x, cy y, r "0.2" ] []


drawLine : Coordinate -> Coordinate -> Svg Msg
drawLine c1 c2 =
    let
        ( x, y ) =
            getSvgCoords c1

        ( xx, yy ) =
            getSvgCoords c2
    in
        line
            [ x1 x
            , y1 y
            , x2 xx
            , y2 yy
            , stroke "black"
            , strokeWidth "0.1"
            ]
            []
