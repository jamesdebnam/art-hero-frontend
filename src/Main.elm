module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
  Browser.sandbox { init = init, update = update, view = view }

type Color = Red | Black | Blue | Green | Yellow | White
type alias PixelGrid = List (List Color)

type alias Model = {
            activeColor: Color,
            pixelGrid: PixelGrid
            }

init : Model
init = {activeColor=  Red, pixelGrid =  List.repeat 6 (List.repeat 6 White) }


type Msg
  = UpdateActiveColor Color
  | PaintPixel Color PixelCoords


type alias PixelCoords = (Int, Int)
update : Msg -> Model -> (Model)
update msg model =
    case msg of
        UpdateActiveColor color ->
            {model | activeColor = color}

        PaintPixel color pixelCoords ->
            {model | }




view model =
  div [][]