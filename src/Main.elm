module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Debug
import Html exposing (Html, button, div, h1, text)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main =
  Browser.sandbox { init = init, update = update, view = view }

type Color = Red | Black | Blue | Green | Yellow | White
type alias PixelCoords = (Int, Int)
type alias PixelMap = Dict PixelCoords Color

type alias Model = {
            activeColor: Color,
            pixelMap: PixelMap
          }

init : Model
init = {activeColor=  Red, pixelMap = Dict.empty }


type Msg
  = UpdateActiveColor Color
  | PaintPixel Color PixelCoords


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateActiveColor color ->
            {model | activeColor = color}

        PaintPixel color pixelCoords ->
            Debug.log "painting"
            {model | pixelMap = Dict.insert pixelCoords color model.pixelMap}


--get_color: Color -> String
--get_color color =
--    case color of
--        Red -> '#ff5733'
--


view_pixel_grid: Model -> Html Msg
view_pixel_grid model = div [
    style "display" "flex",
    style "flex-direction" "column"
    ]  (List.indexedMap (\row items ->
                                    div [
                                            style "display" "flex",
                                            style "flex-direction" "row"
                                    ] (
                                        List.indexedMap (\col _ ->
                                            div
                                                [ style "background-color" "white"
                                                , style "border" "1px solid black"
                                                , style "height" "30px"
                                                , style "width" "30px"
                                                , onClick (PaintPixel model.activeColor (col, row))
                                                ]
                                                []
                                            )  items
                                        ) ) (List.repeat 6 (List.repeat 6 0)))





view : Model -> Html Msg
view model =
  div []
    [
      h1 [ onClick (PaintPixel Blue (0,0)) ] [ text "this is a page"],
      h1 [ onClick (PaintPixel Red (1,1)) ] [ text "this is also a page"],
      view_pixel_grid model
    ]