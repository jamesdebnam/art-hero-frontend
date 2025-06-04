module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onMouseDown, onMouseOver, onMouseUp)


main =
    Browser.sandbox { init = init, update = update, view = view }


type Color
    = Red
    | Black
    | Blue
    | Green
    | Yellow
    | White


type alias PixelCoords =
    ( Int, Int )


type alias PixelMap =
    Dict PixelCoords Color


type alias History =
    List PixelMap


type alias Model =
    { mouseIsDown : Bool
    , activeColor : Color
    , pixelMap : PixelMap
    , history : History
    , future : History
    }


init : Model
init =
    { mouseIsDown = False, activeColor = Red, pixelMap = Dict.empty, history = [ Dict.empty ], future = [ Dict.empty ] }


type Msg
    = UpdateMouseDown Bool
    | UpdateActiveColor Color
    | PaintPixel Color PixelCoords
    | Undo
    | Redo


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateMouseDown mouseIsDown ->
            { model | mouseIsDown = mouseIsDown }

        UpdateActiveColor color ->
            { model | activeColor = color }

        PaintPixel color pixelCoords ->
            let
                newPixelMap =
                    Dict.insert pixelCoords color model.pixelMap
            in
            { model
                | pixelMap = newPixelMap
                , history = model.pixelMap :: model.history
            }

        Undo ->
            case model.history of
                previousState :: restOfTheList ->
                    { model
                        | pixelMap = previousState
                        , history = restOfTheList
                        , future = model.pixelMap :: model.future
                    }

                _ ->
                    model

        Redo ->
            case model.future of
                redo :: restOfTheList ->
                    { model
                        | pixelMap = redo
                        , future = restOfTheList
                        , history = model.pixelMap :: model.history
                    }

                _ ->
                    model


get_pixel_color_from_coords : PixelCoords -> Model -> String
get_pixel_color_from_coords coords model =
    Dict.get coords model.pixelMap |> get_color


get_color : Maybe Color -> String
get_color color =
    case color of
        Nothing ->
            "white"

        Just definedColor ->
            case definedColor of
                Red ->
                    "#ff5733"

                Blue ->
                    "blue"

                Black ->
                    "black"

                Green ->
                    "green"

                Yellow ->
                    "yellow"

                White ->
                    "white"


view_pixel_grid : Model -> Html Msg
view_pixel_grid model =
    div
        [ onMouseDown (UpdateMouseDown True)
        , onMouseUp (UpdateMouseDown False)
        ]
        [ div [ class "column" ]
            (List.indexedMap
                (\row items ->
                    div [ class "row" ]
                        (List.indexedMap
                            (\col _ ->
                                div
                                    [ style "background-color" (get_pixel_color_from_coords ( col, row ) model)
                                    , class "pixel"
                                    , onClick (PaintPixel model.activeColor ( col, row ))
                                    ]
                                    []
                            )
                            items
                        )
                )
                (List.repeat 100 (List.repeat 100 0))
            )
        ]


view_color_button : Model -> Html Msg
view_color_button model =
    div
        [ class "color-picker"
        ]
        ([ p [] [ text "Get yer colors!" ] ]
            ++ List.map
                (\color ->
                    button
                        [ onClick (UpdateActiveColor color)
                        , style "background-color" (colorToString color)
                        , class "color-button"
                        ]
                        [ text "" ]
                )
                [ Red, Black, Blue, Green, Yellow, White ]
        )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "ART HERO!!!!" ]
        , div [ class "row-bottom" ]
            [ view_color_button model
            , div [ class "column" ]
                [ button [ onClick Undo ] [ text "Undo" ]
                , button [ onClick Redo ] [ text "Redo" ]
                , view_pixel_grid model
                ]
            ]
        ]


colorToString : Color -> String
colorToString color =
    case color of
        Red ->
            "Red"

        Black ->
            "Black"

        Blue ->
            "Blue"

        Green ->
            "Green"

        Yellow ->
            "Yellow"

        White ->
            "White"
