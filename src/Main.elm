module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
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


update : Msg -> Model -> (Model)
update msg model =
    case msg of
        UpdateActiveColor color ->
            {model | activeColor = color}

        PaintPixel color pixelCoords ->
            {model | pixelMap = Dict.insert pixelCoords color model.pixelMap }




view model =
  div [][]