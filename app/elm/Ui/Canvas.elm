module Ui.Canvas exposing (Size, view)

import Color
import Css exposing (pct)
import Environment.Line exposing (Line)
import Environment.Turtle exposing (Turtle)
import Html.Styled as H exposing (Html)
import Math.Matrix4 as Mat4
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Svg.Styled as Svg exposing (Svg, svg)
import Svg.Styled.Attributes as A
import Ui.Machine.Environment as Environment exposing (Environment, Object(..))


type alias Size =
    { width : Int
    , height : Int
    }


pointToString : Vec2 -> String
pointToString p =
    let
        { x, y } =
            Vec2.toRecord p
    in
    String.fromFloat x ++ "," ++ String.fromFloat y


pointsToString : List Vec3 -> String
pointsToString =
    List.map
        (\v ->
            let
                { x, y } =
                    Vec3.toRecord v
            in
            [ String.fromFloat x, String.fromFloat y ] |> String.join ","
        )
        >> String.join " "


line : Line -> Svg msg
line { start, end, color } =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    Svg.polyline
        [ A.fill "none"
        , A.stroke <|
            "rgba("
                ++ String.fromFloat (red * 255.0)
                ++ ","
                ++ String.fromFloat (green * 255.0)
                ++ ","
                ++ String.fromFloat (blue * 255.0)
                ++ ","
                ++ String.fromFloat alpha
                ++ ")"
        , A.points
            ([ start, end ]
                |> List.map pointToString
                |> String.join " "
            )
        ]
        []


object : Object -> Svg msg
object object_ =
    case object_ of
        Environment.Line line_ ->
            line line_


lines : List Object -> List (Svg msg)
lines objects_ =
    List.map object objects_


turtle : Turtle -> Svg msg
turtle { x, y, direction } =
    let
        matrix =
            Mat4.makeTranslate3 x y 0
                |> Mat4.rotate direction Vec3.k
                |> Mat4.translate3 0 1 0

        transform =
            Mat4.transform matrix

        p1 : { x : Float, y : Float, z : Float }
        p1 =
            { x = -15, y = 0, z = 0 }

        p2 =
            { x = 15, y = 0, z = 0 }

        p3 =
            { x = 0, y = -30, z = 0 }

        points_ =
            [ p1, p2, p3 ]
                |> List.map (Vec3.fromRecord >> transform)
                |> pointsToString
    in
    Svg.polygon
        [ A.fill "none"
        , A.stroke "black"
        , A.points points_
        ]
        []


view : Size -> Environment -> Html msg
view { width, height } env =
    let
        viewBox_ =
            [ -(width // 2), -(height // 2), width, height ]
                |> List.map String.fromInt
                |> String.join " "

        objects =
            turtle env.turtle :: lines env.objects
    in
    H.div
        [ A.css [ Css.width (pct 100), Css.height (pct 100) ]
        , A.viewBox viewBox_
        ]
        [ svg
            [ A.css [ Css.width (pct 100), Css.height (pct 95) ]
            , A.viewBox viewBox_
            ]
            objects
        ]
