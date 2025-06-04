module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Debug
import Html exposing (Html, button, div, h1, text)
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





view : Model -> Html Msg
view model =
  div []
    [
      h1 [ onClick (PaintPixel Blue (0,0)) ] [ text "this is a page"],
      h1 [ onClick (PaintPixel Red (1,1)) ] [ text "this is also a page"]
    ]