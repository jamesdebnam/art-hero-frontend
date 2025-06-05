module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onMouseDown, onMouseOver, onMouseUp)


main =
    Browser.sandbox { init = init, update = update, view = view }


maxHistorySize : Int
maxHistorySize =
    100


maxFutureSize : Int
maxFutureSize =
    100


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


type MouseAction
    = Clicked
    | MouseOver


type Msg
    = UpdateMouseDown Bool
    | UpdateActiveColor Color
    | PaintPixel Color PixelCoords MouseAction
    | Undo
    | Redo


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateMouseDown mouseIsDown ->
            case mouseIsDown of
                True ->
                    { model | mouseIsDown = mouseIsDown }

                False ->
                    { model | mouseIsDown = mouseIsDown, history = model.pixelMap :: model.history }

        UpdateActiveColor color ->
            { model | activeColor = color }

        PaintPixel color pixelCoords mouseAction ->
            case ( mouseAction, model.mouseIsDown ) of
                ( Clicked, _ ) ->
                    let
                        newPixelMap =
                            Dict.insert pixelCoords color model.pixelMap

                        newHistory =
                            model.pixelMap :: model.history
                    in
                    { model
                        | pixelMap = newPixelMap
                        , history = List.take maxHistorySize newHistory
                        , future = []
                    }

                ( MouseOver, True ) ->
                    let
                        newPixelMap =
                            Dict.insert pixelCoords color model.pixelMap
                    in
                    { model
                        | pixelMap = newPixelMap
                        , future = []
                    }

                ( MouseOver, False ) ->
                    model

        Undo ->
            case model.history of
                currentState :: previousState :: restOfTheList ->
                    let
                        newFuture =
                            currentState :: model.future
                    in
                    { model
                        | pixelMap = previousState
                        , history = previousState :: restOfTheList
                        , future = List.take maxFutureSize newFuture
                    }

                _ ->
                    model

        Redo ->
            case model.future of
                futureState :: restOfTheList ->
                    let
                        newHistory =
                            futureState :: model.history
                    in
                    { model
                        | pixelMap = futureState
                        , future = restOfTheList
                        , history = List.take maxHistorySize newHistory
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
                                    , onClick (PaintPixel model.activeColor ( col, row ) Clicked)
                                    , onMouseOver (PaintPixel model.activeColor ( col, row ) MouseOver)
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
                        , class
                            (if model.activeColor == color then
                                "color-button active-button"

                             else
                                "color-button"
                            )
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
                [ div [class "button-row"] [ button

                    [ onClick Undo
                    , class
                        (if List.length model.history > 0 then
                            "button"

                         else
                            "button button-disabled"
                        )
                    ]
                    [ text "Undo" ]
                , button
                    [ onClick Redo
                    , class
                        (if List.length model.future > 0 then
                            "button"
                         else
                            "button button-disabled"
                        )
                    ]
                    [ text "Redo" ]]
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
