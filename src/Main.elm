module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, type_, id, for, value)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (combine)
import Svg exposing (Svg, svg, g, circle, line, path)
import Svg.Attributes exposing (cx, cy, r, x1, x2, y1, y2, d)
import Svg.Attributes exposing (height, width, stroke, strokeWidth, viewBox, fill)


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
    , debug : Bool
    }


initialExpr : String
initialExpr =
    "1,1;2,7;7,7,6;8,1"


model : Model
model =
    Model
        (Maybe.withDefault [] (parseInput initialExpr))
        False



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
                { model | coordinates = coordinates }

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
        [ h1 [] [ text "FontoBene Visualizer" ]
        , p []
            [ text "Enter a FontoBene path expression!" ]
        , div []
            [ input
                [ type_ "text"
                , placeholder "Coordinates"
                , onInput CoordinateString
                , value initialExpr
                ]
                []
            ]
        , div []
            [ input [ id "cbxDebug", type_ "checkbox", onInput ToggleDebug ] []
            , label [ for "cbxDebug" ] [ text "Debug" ]
            ]
        , div []
            [ p [] [ text <| (toString model.coordinates) ++ ", debug? " ++ (toString model.debug) ]
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


drawPolyline : Bool -> List Coordinate -> Svg Msg
drawPolyline debug coords =
    g [] <|
        (case List.head coords of
            Just coord ->
                [ drawPoint coord ]

            Nothing ->
                []
        )
            ++ (List.map2 (drawCoordinate debug) coords (List.drop 1 coords))


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
        circle [ cx (toString x), cy (toString y), r "0.2" ] []


drawLine : Bool -> Coordinate -> Coordinate -> Svg Msg
drawLine debug prevCoord coord =
    let
        ( x, y ) =
            getCoords prevCoord
    in
        case coord of
            Point xx yy ->
                line
                    [ x1 (x |> toString)
                    , y1 (y |> toString)
                    , x2 (xx |> toString)
                    , y2 (yy |> fixY |> toString)
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
                        toString <| degreesToRadius deg length

                    midX =
                        (x + xx) / 2

                    midY =
                        (y + yy) / 2

                    orthVecX =
                        (yy - y) / length

                    orthVecY =
                        -(xx - x) / length

                    orthVecLen =
                        (length / 2) / tan ((degreesToRadians deg) / 2)

                    centerX =
                        (xx + x) / 2 + orthVecX * orthVecLen

                    centerY =
                        (yy + y) / 2 + orthVecY * orthVecLen
                in
                    g [] <|
                        [ path
                            [ d
                                ("M"
                                    ++ (toString x)
                                    ++ ","
                                    ++ (toString y)
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
                                    ++ (toString xx)
                                    ++ ","
                                    ++ (toString yy)
                                )
                            , fill "none"
                            , stroke "black"
                            , strokeWidth "0.1"
                            ]
                            []
                        ]
                            ++ if not debug then
                                []
                               else
                                [ line
                                    [ x1 (x |> toString)
                                    , y1 (y |> toString)
                                    , x2 (centerX |> toString)
                                    , y2 (centerY |> toString)
                                    , stroke "red"
                                    , strokeWidth "0.1"
                                    ]
                                    []
                                , line
                                    [ x1 (xx |> toString)
                                    , y1 (yy |> toString)
                                    , x2 (centerX |> toString)
                                    , y2 (centerY |> toString)
                                    , stroke "blue"
                                    , strokeWidth "0.1"
                                    ]
                                    []
                                , line
                                    [ x1 ((x + xx) / 2 |> toString)
                                    , y1 ((y + yy) / 2 |> toString)
                                    , x2 (centerX |> toString)
                                    , y2 (centerY |> toString)
                                    , stroke "green"
                                    , strokeWidth "0.05"
                                    ]
                                    []
                                , circle
                                    [ cx (toString midX)
                                    , cy (toString midY)
                                    , r "0.2"
                                    , fill "green"
                                    ]
                                    []
                                ]


{-| Convert degrees to radians.
-}
degreesToRadians : Float -> Float
degreesToRadians deg =
    deg * pi / 180


{-| r = \frac{s}{2 \cdot sin(\alpha / 2)}
-}
degreesToRadius : Float -> Float -> Float
degreesToRadius deg length =
    length / (2 * sin ((degreesToRadians deg) / 2))
