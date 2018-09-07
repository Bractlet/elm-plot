port module Docs exposing (Model, examples, highlight, init, main, toUrl, update, updateFocused, view, viewCode, viewExample, viewFooter, viewHeader, viewLink, viewToggleText, viewToggler, visibilityClass)

import Common exposing (..)
import Html exposing (Html, a, br, code, div, h1, header, img, p, pre, span, text)
import Html.Attributes exposing (class, classList, href, id, name, src, style)
import Html.Events exposing (onClick)
import Msg exposing (..)
import Plot exposing (Point)
import PlotAxis
import PlotBars
import PlotRangeFrame
import PlotSine



-- MODEL


type alias Model =
    { focused : Maybe String
    , rangeFrameHover : Maybe Point
    , barsHover : Maybe Point
    }


init : Model
init =
    { focused = Nothing
    , rangeFrameHover = Nothing
    , barsHover = Nothing
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ focused } as model) =
    case msg of
        FocusExample id ->
            ( { model | focused = updateFocused id focused }
            , Cmd.none
            )

        HoverRangeFrame point ->
            ( { model | rangeFrameHover = point }
            , Cmd.none
            )

        HoverBars point ->
            ( { model | barsHover = point }
            , Cmd.none
            )


updateFocused : String -> Maybe String -> Maybe String
updateFocused newId model =
    if Just newId == model then
        Nothing

    else
        Just newId



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "view" ]
        [ div [ class "view--left" ] [ viewHeader ]
        , div [ class "view--right" ] (List.map (viewExample model) (examples model))
        ]


viewHeader : Html msg
viewHeader =
    header [ class "view-header" ]
        [ h1 [ class "view-header__title" ] [ text "elm-plot" ]
        , p []
            [ a [ href "https://github.com/terezka/elm-plot" ] [ text "github" ]
            , span [] [ text " / " ]
            , a [ href "https://twitter.com/terezk_a" ] [ text "twitter" ]
            ]
        ]



-- VIEW EXAMPLE


viewExample : Model -> PlotExample Msg -> Html.Html Msg
viewExample model ({ title, id, view, code } as example) =
    div [ class ("view-plot " ++ visibilityClass model id) ]
        [ div [ class "view-plot--left" ] [ view ]
        , div [ class "view-plot--right" ]
            [ viewCode model example
            , viewFooter model example
            ]
        ]


viewFooter : Model -> PlotExample msg -> Html Msg
viewFooter model { title, id } =
    div [ class "view-footer" ]
        [ viewToggler model id
        ]


viewToggler : Model -> String -> Html.Html Msg
viewToggler model id =
    p [ class "view-toggler" ]
        [ a [ onClick (FocusExample id) ] [ viewToggleText model id ]
        , span [] [ text " / " ]
        , viewLink id
        ]


viewToggleText : Model -> String -> Html msg
viewToggleText { focused } id =
    if focused == Just id then
        text "hide source"

    else
        text "view source"


viewCode : Model -> PlotExample msg -> Html Msg
viewCode model { id, code } =
    div [ class "view-code" ]
        [ Html.code [ class "elm view-code__inner" ] [ pre [] [ text code ] ]
        , viewLink id
        ]


viewLink : String -> Html.Html Msg
viewLink id =
    a [ class "view-link", href (toUrl id) ]
        [ text "full source" ]



-- View helpers


toUrl : String -> String
toUrl end =
    "https://github.com/terezka/elm-plot/blob/master/docs/src/" ++ end ++ ".elm"


visibilityClass : Model -> String -> String
visibilityClass { focused } id =
    if focused == Just id then
        "view-plot__open"

    else
        "view-plot__closed"



-- Ports


port highlight : () -> Cmd msg



-- Main


examples : Model -> List (PlotExample Msg)
examples model =
    [ PlotRangeFrame.plotExample model.rangeFrameHover
    , PlotSine.plotExample
    , PlotAxis.plotExample
    , PlotBars.plotExample model.barsHover
    ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, highlight () )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
