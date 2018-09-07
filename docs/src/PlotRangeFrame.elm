module PlotRangeFrame exposing (plotExample)

import Common exposing (..)
import Msg exposing (..)
import Plot exposing (..)
import Svg exposing (Svg)
import Svg.Attributes exposing (fill, r, stroke, strokeDasharray, strokeWidth)
import Svg.Events exposing (onMouseOut, onMouseOver)


plotExample : Maybe Point -> PlotExample Msg
plotExample hinted =
    { title = "PlotRangeFrame"
    , code = code
    , view = view hinted
    , id = "PlotRangeFrame"
    }


data : List ( Float, Float )
data =
    [ ( 1.31, 240 )
    , ( 1.31, 238.2 )
    , ( 1.324, 237.4 )
    , ( 1.33, 239.7 )
    , ( 1.347, 238.9 )
    , ( 1.35, 236.5 )
    , ( 1.369, 236.6 )
    , ( 1.37, 238 )
    , ( 1.378, 237 )
    , ( 1.364, 237.6 )
    , ( 1.366, 236.4 )
    , ( 1.33, 238.3 )
    , ( 1.324, 237.4 )
    , ( 1.33, 238.7 )
    , ( 1.347, 236.9 )
    , ( 1.35, 237.5 )
    , ( 1.369, 237.6 )
    , ( 1.37, 238.4 )
    , ( 1.378, 237.3 )
    , ( 1.406, 233.7 )
    , ( 1.364, 236.6 )
    , ( 1.366, 238.4 )
    , ( 1.33, 235.3 )
    , ( 1.395, 233.7 )
    , ( 1.405, 234.7 )
    , ( 1.41, 232.7 )
    ]


scatter : Maybe Point -> Series (List ( Float, Float )) Msg
scatter hinting =
    { axis = rangeFrameAxis hinting .y
    , interpolation = None
    , toDataPoints = List.map (rangeFrameHintDot hinting)
    }


circle : Float -> Float -> Svg Msg
circle x y =
    Svg.circle
        [ r "5"
        , stroke "transparent"
        , strokeWidth "3px"
        , fill pinkStroke
        , onMouseOver (HoverRangeFrame (Just { x = x, y = y }))
        , onMouseOut (HoverRangeFrame Nothing)
        ]
        []


flashyLine : Float -> Float -> Point -> Maybe (AxisSummary -> LineCustomizations)
flashyLine x y hinted =
    if hinted.x == x && hinted.y == y then
        Just (fullLine [ stroke "#a3a3a3", strokeDasharray "2, 10" ])

    else
        Nothing


rangeFrameHintDot : Maybe Point -> ( Float, Float ) -> DataPoint Msg
rangeFrameHintDot hinted ( x, y ) =
    { view = Just (circle x y)
    , xLine = Maybe.andThen (flashyLine x y) hinted
    , yLine = Maybe.andThen (flashyLine x y) hinted
    , xTick = Just (simpleTick x)
    , yTick = Just (simpleTick y)
    , hint = Nothing
    , x = x
    , y = y
    }


rangeFrameAxis : Maybe Point -> (Point -> Float) -> Axis
rangeFrameAxis hinted toValue =
    customAxis <|
        \summary ->
            { position = closestToZero
            , axisLine = Nothing
            , ticks = List.map simpleTick [ summary.dataMin, summary.dataMax ]
            , labels =
                List.map simpleLabel [ summary.dataMin, summary.dataMax ]
                    ++ hintLabel hinted toValue
            , flipAnchor = False
            }


hintLabel : Maybe Point -> (Point -> Float) -> List LabelCustomizations
hintLabel hinted toValue =
    hinted
        |> Maybe.map (toValue >> simpleLabel >> List.singleton)
        |> Maybe.withDefault []


view : Maybe Point -> Svg.Svg Msg
view hinting =
    viewSeriesCustom
        { defaultSeriesPlotCustomizations
            | horizontalAxis = rangeFrameAxis hinting .x
            , margin = { top = 20, bottom = 20, left = 50, right = 40 }
            , toRangeLowest = \y -> y - 0.01
            , toDomainLowest = \y -> y - 1
        }
        [ scatter hinting ]
        data


code : String
code =
    """
scatter : Maybe Point -> Series (List ( Float, Float )) Msg
scatter hinting =
  { axis = rangeFrameAxis hinting .y
  , interpolation = None
  , toDataPoints = List.map (rangeFrameHintDot hinting)
  }


circle : Float -> Float -> Svg Msg
circle x y =
  Svg.circle
    [ r "5"
    , stroke "transparent"
    , strokeWidth "3px"
    , fill pinkStroke
    , onMouseOver (HoverRangeFrame (Just { x = x, y = y }))
    , onMouseOut (HoverRangeFrame Nothing)
    ]
    []


flashyLine : Float -> Float -> Point -> Maybe (AxisSummary -> LineCustomizations)
flashyLine x y hinted =
  if hinted.x == x && hinted.y == y then
    Just (fullLine [ stroke "#a3a3a3", strokeDasharray "2, 10" ])
  else
    Nothing


rangeFrameHintDot : Maybe Point -> ( Float, Float ) -> DataPoint Msg
rangeFrameHintDot hinted ( x, y ) =
  { view = Just (circle x y)
  , xLine = Maybe.andThen (flashyLine x y) hinted
  , yLine = Maybe.andThen (flashyLine x y) hinted
  , xTick = Just (simpleTick x)
  , yTick = Just (simpleTick y)
  , hint = Nothing
  , x = x
  , y = y
  }


rangeFrameAxis : Maybe Point -> (Point -> Float) -> Axis
rangeFrameAxis hinted toValue =
  customAxis <| \\summary ->
    { position = closestToZero
    , axisLine = Nothing
    , ticks = List.map simpleTick [ summary.dataMin, summary.dataMax ]
    , labels = List.map simpleLabel [ summary.dataMin, summary.dataMax ]
        ++ hintLabel hinted toValue
    , flipAnchor = False
    }


hintLabel : Maybe Point -> (Point -> Float) -> List LabelCustomizations
hintLabel hinted toValue =
  hinted
    |> Maybe.map (toValue >> simpleLabel >> List.singleton)
    |> Maybe.withDefault []


view : Maybe Point -> Svg.Svg Msg
view hinting =
  viewSeriesCustom
    { defaultSeriesPlotCustomizations
    | horizontalAxis = rangeFrameAxis hinting .x
    , margin = { top = 20, bottom = 20, left = 50, right = 40 }
    , toRangeLowest = \\y -> y - 0.01
    , toDomainLowest = \\y -> y - 1
    }
    [ scatter hinting ]
    data
"""
