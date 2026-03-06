module SvgPathToSegments exposing (Coords, Segment, parsePath, sampleAt, totalLength)

{-| Convert SVG path strings into segments using elm-geometry.
-}

import Angle
import CubicSpline2d
import Direction2d
import EllipticalArc2d
import Length exposing (Meters)
import LineSegment2d
import Path.LowLevel as LowLevel exposing (Coordinate, DrawTo(..), Mode(..), MoveTo(..))
import Path.LowLevel.Parser
import Point2d exposing (Point2d)
import QuadraticSpline2d
import SweptAngle


type Coords
    = Coords


type Segment
    = Line (LineSegment2d.LineSegment2d Meters Coords)
    | Cubic (CubicSpline2d.ArcLengthParameterized Meters Coords)
    | Quadratic (QuadraticSpline2d.ArcLengthParameterized Meters Coords)
    | Arc (EllipticalArc2d.ArcLengthParameterized Meters Coords)


type alias CursorState =
    { cursor : Coordinate
    , start : Coordinate
    , lastCubicCP : Maybe Coordinate
    , lastQuadraticCP : Maybe Coordinate
    }


maxError : Length.Length
maxError =
    Length.meters 0.01


pt : Coordinate -> Point2d Meters Coords
pt ( x, y ) =
    Point2d.meters x y


resolve : Mode -> Coordinate -> Coordinate -> Coordinate
resolve mode ( cx, cy ) ( x, y ) =
    case mode of
        Absolute ->
            ( x, y )

        Relative ->
            ( cx + x, cy + y )


reflect : Coordinate -> Maybe Coordinate -> Coordinate
reflect ( cx, cy ) cp =
    case cp of
        Just ( cpx, cpy ) ->
            ( 2 * cx - cpx, 2 * cy - cpy )

        Nothing ->
            ( cx, cy )


clearCP : CursorState -> Coordinate -> CursorState
clearCP st target =
    { st | cursor = target, lastCubicCP = Nothing, lastQuadraticCP = Nothing }



-- Segment constructors


makeLine : Coordinate -> Coordinate -> Maybe Segment
makeLine from to =
    let
        seg =
            LineSegment2d.from (pt from) (pt to)
    in
    if Length.inMeters (LineSegment2d.length seg) < 1.0e-10 then
        Nothing

    else
        Just (Line seg)


makeCubic : Coordinate -> Coordinate -> Coordinate -> Coordinate -> Maybe Segment
makeCubic p1 p2 p3 p4 =
    CubicSpline2d.fromControlPoints (pt p1) (pt p2) (pt p3) (pt p4)
        |> CubicSpline2d.nondegenerate
        |> Result.toMaybe
        |> Maybe.map
            (CubicSpline2d.arcLengthParameterized { maxError = maxError } >> Cubic)


makeQuadratic : Coordinate -> Coordinate -> Coordinate -> Maybe Segment
makeQuadratic p1 p2 p3 =
    QuadraticSpline2d.fromControlPoints (pt p1) (pt p2) (pt p3)
        |> QuadraticSpline2d.nondegenerate
        |> Result.toMaybe
        |> Maybe.map
            (QuadraticSpline2d.arcLengthParameterized { maxError = maxError } >> Quadratic)


makeArc : Coordinate -> LowLevel.EllipticalArcArgument -> Maybe Segment
makeArc from arg =
    let
        ( rx, ry ) =
            arg.radii

        sweptAngle =
            case ( arg.arcFlag, arg.direction ) of
                ( LowLevel.SmallestArc, LowLevel.Clockwise ) ->
                    SweptAngle.smallNegative

                ( LowLevel.SmallestArc, LowLevel.CounterClockwise ) ->
                    SweptAngle.smallPositive

                ( LowLevel.LargestArc, LowLevel.Clockwise ) ->
                    SweptAngle.largeNegative

                ( LowLevel.LargestArc, LowLevel.CounterClockwise ) ->
                    SweptAngle.largePositive
    in
    EllipticalArc2d.fromEndpoints
        { startPoint = pt from
        , endPoint = pt arg.target
        , xRadius = Length.meters (abs rx)
        , yRadius = Length.meters (abs ry)
        , xDirection = Direction2d.fromAngle (Angle.degrees arg.xAxisRotate)
        , sweptAngle = sweptAngle
        }
        |> Maybe.andThen (EllipticalArc2d.nondegenerate >> Result.toMaybe)
        |> Maybe.map
            (EllipticalArc2d.arcLengthParameterized { maxError = maxError } >> Arc)



-- Sampling


segmentLength : Segment -> Float
segmentLength seg =
    Length.inMeters
        (case seg of
            Line lineseg ->
                LineSegment2d.length lineseg

            Cubic param ->
                CubicSpline2d.arcLength param

            Quadratic param ->
                QuadraticSpline2d.arcLength param

            Arc param ->
                EllipticalArc2d.arcLength param
        )


sampleSegment : Segment -> Float -> ( Point2d Meters Coords, Direction2d.Direction2d Coords )
sampleSegment seg distance =
    case seg of
        Line lineseg ->
            ( LineSegment2d.interpolate lineseg (distance / segmentLength seg)
            , LineSegment2d.direction lineseg
                |> Maybe.withDefault Direction2d.x
            )

        Cubic param ->
            let
                d =
                    Length.meters distance
            in
            ( CubicSpline2d.pointAlong param d
            , CubicSpline2d.tangentDirectionAlong param d
            )

        Quadratic param ->
            let
                d =
                    Length.meters distance
            in
            ( QuadraticSpline2d.pointAlong param d
            , QuadraticSpline2d.tangentDirectionAlong param d
            )

        Arc param ->
            let
                d =
                    Length.meters distance
            in
            ( EllipticalArc2d.pointAlong param d
            , EllipticalArc2d.tangentDirectionAlong param d
            )



-- Processing draw commands


addSegment : Maybe Segment -> List Segment -> List Segment
addSegment maybeSeg acc =
    case maybeSeg of
        Just seg ->
            seg :: acc

        Nothing ->
            acc


lineStep : Mode -> Coordinate -> ( CursorState, List Segment ) -> ( CursorState, List Segment )
lineStep mode target ( st, acc ) =
    let
        resolved =
            resolve mode st.cursor target
    in
    ( clearCP st resolved, addSegment (makeLine st.cursor resolved) acc )


processDrawTo : DrawTo -> ( CursorState, List Segment ) -> ( CursorState, List Segment )
processDrawTo drawTo ( state, segs ) =
    case drawTo of
        LineTo mode coords ->
            List.foldl (lineStep mode) ( state, segs ) coords

        Horizontal mode values ->
            List.foldl
                (\val acc ->
                    let
                        ( cx, cy ) =
                            (Tuple.first acc).cursor
                    in
                    lineStep Absolute
                        ( case mode of
                            Absolute ->
                                val

                            Relative ->
                                cx + val
                        , cy
                        )
                        acc
                )
                ( state, segs )
                values

        Vertical mode values ->
            List.foldl
                (\val acc ->
                    let
                        ( cx, cy ) =
                            (Tuple.first acc).cursor
                    in
                    lineStep Absolute
                        ( cx
                        , case mode of
                            Absolute ->
                                val

                            Relative ->
                                cy + val
                        )
                        acc
                )
                ( state, segs )
                values

        CurveTo mode triples ->
            List.foldl
                (\( c1, c2, to ) ( st, acc ) ->
                    let
                        cp1 =
                            resolve mode st.cursor c1

                        cp2 =
                            resolve mode st.cursor c2

                        target =
                            resolve mode st.cursor to
                    in
                    ( { st | cursor = target, lastCubicCP = Just cp2, lastQuadraticCP = Nothing }
                    , addSegment (makeCubic st.cursor cp1 cp2 target) acc
                    )
                )
                ( state, segs )
                triples

        SmoothCurveTo mode pairs ->
            List.foldl
                (\( c2, to ) ( st, acc ) ->
                    let
                        cp1 =
                            reflect st.cursor st.lastCubicCP

                        cp2 =
                            resolve mode st.cursor c2

                        target =
                            resolve mode st.cursor to
                    in
                    ( { st | cursor = target, lastCubicCP = Just cp2, lastQuadraticCP = Nothing }
                    , addSegment (makeCubic st.cursor cp1 cp2 target) acc
                    )
                )
                ( state, segs )
                pairs

        QuadraticBezierCurveTo mode pairs ->
            List.foldl
                (\( c1, to ) ( st, acc ) ->
                    let
                        cp =
                            resolve mode st.cursor c1

                        target =
                            resolve mode st.cursor to
                    in
                    ( { st | cursor = target, lastCubicCP = Nothing, lastQuadraticCP = Just cp }
                    , addSegment (makeQuadratic st.cursor cp target) acc
                    )
                )
                ( state, segs )
                pairs

        SmoothQuadraticBezierCurveTo mode coords ->
            List.foldl
                (\to ( st, acc ) ->
                    let
                        cp =
                            reflect st.cursor st.lastQuadraticCP

                        target =
                            resolve mode st.cursor to
                    in
                    ( { st | cursor = target, lastCubicCP = Nothing, lastQuadraticCP = Just cp }
                    , addSegment (makeQuadratic st.cursor cp target) acc
                    )
                )
                ( state, segs )
                coords

        EllipticalArc mode args ->
            List.foldl
                (\arg ( st, acc ) ->
                    let
                        target =
                            resolve mode st.cursor arg.target
                    in
                    ( clearCP st target
                    , addSegment (makeArc st.cursor { arg | target = target }) acc
                    )
                )
                ( state, segs )
                args

        ClosePath ->
            ( clearCP state state.start
            , addSegment (makeLine state.cursor state.start) segs
            )


processSubPath : LowLevel.SubPath -> List Segment
processSubPath { moveto, drawtos } =
    let
        (MoveTo _ start) =
            moveto
    in
    List.foldl processDrawTo
        ( { cursor = start, start = start, lastCubicCP = Nothing, lastQuadraticCP = Nothing }, [] )
        drawtos
        |> Tuple.second
        |> List.reverse



-- Public API


totalLength : List Segment -> Float
totalLength =
    List.foldl (\seg acc -> acc + segmentLength seg) 0


parsePath : String -> Maybe (List (List Segment))
parsePath svgPathString =
    case Path.LowLevel.Parser.parse svgPathString of
        Ok subPaths ->
            let
                paths =
                    List.filterMap
                        (\subPath ->
                            case processSubPath subPath of
                                [] ->
                                    Nothing

                                segments ->
                                    Just segments
                        )
                        subPaths
            in
            if List.isEmpty paths then
                Nothing

            else
                Just paths

        Err _ ->
            Nothing


sampleAt :
    List Segment
    -> Float
    -> Maybe ( Point2d Meters Coords, Direction2d.Direction2d Coords )
sampleAt segments distance =
    case segments of
        [] ->
            Nothing

        seg :: rest ->
            let
                len =
                    segmentLength seg
            in
            if distance <= len || List.isEmpty rest then
                Just (sampleSegment seg (clamp 0 len distance))

            else
                sampleAt rest (distance - len)
