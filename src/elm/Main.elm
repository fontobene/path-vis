module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, input, label, p, text)
import Html.Attributes exposing (checked, for, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (combine)
import Svg exposing (Svg, circle, g, line, path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, r, stroke, strokeWidth, viewBox, width, x1, x2, y1, y2)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type Coordinate
    = Point Float Float
    | Arc Float Float Float


type alias Model =
    { coordinates : List Coordinate
    , debug : Bool
    , expr : String
    }


initialExpr : String
initialExpr =
    "1,1;2,7;7,7,6;8,1"


init : Model
init =
    { coordinates = Maybe.withDefault [] (parseInput initialExpr)
    , debug = False
    , expr = initialExpr
    }



-- UPDATE


type Msg
    = CoordinateString String
    | ToggleDebug String


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
            { model | expr = string, coordinates = coordinates }

        ToggleDebug _ ->
            { model | debug = not model.debug }


parseInput : String -> Maybe (List Coordinate)
parseInput expr =
    let
        parts =
            List.map (String.split ",")
                (String.split ";" expr)
    in
    combine (List.map parseCoordinate parts)


{-| Parse a list of coordinate strings.
-}
parseCoordinate : List String -> Maybe Coordinate
parseCoordinate input =
    case input of
        [ xStr, yStr ] ->
            case ( String.toFloat xStr, String.toFloat yStr ) of
                ( Just x, Just y ) ->
                    Just (Point x y)

                _ ->
                    Nothing

        [ xStr, yStr, aStr ] ->
            case ( String.toFloat xStr, String.toFloat yStr, String.toFloat aStr ) of
                ( Just x, Just y, Just a ) ->
                    Just (Arc x y a)

                _ ->
                    Nothing

        _ ->
            Nothing



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "FontoBene Visualizer" ]
        , p []
            [ text "Enter a FontoBene path expression!" ]
        , div []
            [ input
                [ type_ "text"
                , placeholder "Coordinates"
                , onInput CoordinateString
                , value model.expr
                ]
                []
            ]
        , div []
            [ input [ id "cbxDebug", type_ "checkbox", checked model.debug, onInput ToggleDebug ] []
            , label [ for "cbxDebug" ] [ text "Debug" ]
            ]
        , svg [ height "200", width "200", viewBox "-0.5 -0.5 10 10" ]
            [ makeGrid
            , drawPolyline model.debug model.coordinates
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
                [ x1 <| String.fromInt x
                , y1 "0"
                , x2 <| String.fromInt x
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
                , y1 <| String.fromInt y
                , x2 "9"
                , y2 <| String.fromInt y
                , stroke gridColor
                , strokeWidth "0.1"
                ]
                []
        )
        (List.range 0 9)


drawPolyline : Bool -> List Coordinate -> Svg Msg
drawPolyline debug coords =
    g [] <|
        (case List.head coords of
            Just coord ->
                [ drawPoint coord ]

            Nothing ->
                []
        )
            ++ List.map2 (drawCoordinate debug) coords (List.drop 1 coords)


drawCoordinate : Bool -> Coordinate -> Coordinate -> Svg Msg
drawCoordinate debug prevCoord coord =
    g
        []
        [ drawPoint coord
        , drawLine debug prevCoord coord
        ]


fixY : Float -> Float
fixY val =
    9 - val


getCoords : Coordinate -> ( Float, Float )
getCoords coord =
    case coord of
        Point x y ->
            ( x, fixY y )

        Arc x y _ ->
            ( x, fixY y )


drawPoint : Coordinate -> Svg Msg
drawPoint coord =
    let
        ( x, y ) =
            getCoords coord
    in
    circle [ cx (String.fromFloat x), cy (String.fromFloat y), r "0.2" ] []


drawLine : Bool -> Coordinate -> Coordinate -> Svg Msg
drawLine debug prevCoord coord =
    let
        ( x, y ) =
            getCoords prevCoord
    in
    case coord of
        Point xx yy ->
            line
                [ x1 (x |> String.fromFloat)
                , y1 (y |> String.fromFloat)
                , x2 (xx |> String.fromFloat)
                , y2 (yy |> fixY |> String.fromFloat)
                , stroke "black"
                , strokeWidth "0.1"
                ]
                []

        Arc xx yy_ a ->
            let
                yy =
                    fixY yy_

                deg =
                    a * 20

                length =
                    sqrt ((xx - x) ^ 2 + (yy - y) ^ 2)

                radius =
                    String.fromFloat <| degreesToRadius deg length

                midX =
                    (x + xx) / 2

                midY =
                    (y + yy) / 2

                orthVecX =
                    (yy - y) / length

                orthVecY =
                    -(xx - x) / length

                orthVecLen =
                    (length / 2) / tan (degreesToRadians deg / 2)

                centerX =
                    (xx + x) / 2 + orthVecX * orthVecLen

                centerY =
                    (yy + y) / 2 + orthVecY * orthVecLen
            in
            g [] <|
                [ if deg == 0 then
                    -- Straight line
                    line
                        [ x1 (String.fromFloat x)
                        , y1 (String.fromFloat y)
                        , x2 (String.fromFloat xx)
                        , y2 (String.fromFloat yy)
                        , stroke "black"
                        , strokeWidth "0.1"
                        ]
                        []

                  else
                    path
                        [ d
                            ("M"
                                ++ String.fromFloat x
                                ++ ","
                                ++ String.fromFloat y
                                ++ " A"
                                ++ radius
                                ++ ","
                                ++ radius
                                ++ " 0 0,"
                                ++ (if deg >= 0 then
                                        "0"

                                    else
                                        "1"
                                   )
                                ++ " "
                                ++ String.fromFloat xx
                                ++ ","
                                ++ String.fromFloat yy
                            )
                        , fill "none"
                        , stroke "black"
                        , strokeWidth "0.1"
                        ]
                        []
                ]
                    ++ (if not debug || deg == 0 then
                            []

                        else
                            [ line
                                [ x1 (x |> String.fromFloat)
                                , y1 (y |> String.fromFloat)
                                , x2 (centerX |> String.fromFloat)
                                , y2 (centerY |> String.fromFloat)
                                , stroke "red"
                                , strokeWidth "0.1"
                                ]
                                []
                            , line
                                [ x1 (xx |> String.fromFloat)
                                , y1 (yy |> String.fromFloat)
                                , x2 (centerX |> String.fromFloat)
                                , y2 (centerY |> String.fromFloat)
                                , stroke "blue"
                                , strokeWidth "0.1"
                                ]
                                []
                            , line
                                [ x1 ((x + xx) / 2 |> String.fromFloat)
                                , y1 ((y + yy) / 2 |> String.fromFloat)
                                , x2 (centerX |> String.fromFloat)
                                , y2 (centerY |> String.fromFloat)
                                , stroke "green"
                                , strokeWidth "0.05"
                                ]
                                []
                            , circle
                                [ cx (String.fromFloat midX)
                                , cy (String.fromFloat midY)
                                , r "0.2"
                                , fill "green"
                                ]
                                []
                            ]
                       )


{-| Convert degrees to radians.
-}
degreesToRadians : Float -> Float
degreesToRadians deg =
    deg * pi / 180


{-| r = \\frac{s}{2 \\cdot sin(\\alpha / 2)}
-}
degreesToRadius : Float -> Float -> Float
degreesToRadius deg length =
    length / (2 * sin (degreesToRadians deg / 2))
