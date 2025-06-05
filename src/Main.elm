module Main exposing (..)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onMouseDown, onMouseOver, onMouseUp)
import Http
import Json.Decode as D
import Url


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange =
            \_ ->
                None
        , onUrlRequest =
            \_ ->
                None
        }


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


type FetchState
    = Loading
    | Success MasterpieceList
    | Error


type alias Model =
    { mouseIsDown : Bool
    , activeColor : Color
    , pixelMap : PixelMap
    , history : History
    , future : History
    , fetchState : FetchState
    }


init : Bool -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { mouseIsDown = False
      , activeColor = Red
      , pixelMap = Dict.empty
      , history = [ Dict.empty ]
      , future = [ Dict.empty ]
      , fetchState = Loading
      }
    , getMasterpieces
    )


type MouseAction
    = Clicked
    | MouseOver


type alias FetchedMasterpieces =
    List MasterpieceListItem


type Msg
    = UpdateMouseDown Bool
    | UpdateActiveColor Color
    | PaintPixel Color PixelCoords MouseAction
    | Undo
    | Redo
    | GotMasterpieces (Result Http.Error FetchedMasterpieces)
    | None


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMouseDown mouseIsDown ->
            case mouseIsDown of
                True ->
                    ( { model | mouseIsDown = mouseIsDown }, Cmd.none )

                False ->
                    ( { model | mouseIsDown = mouseIsDown, history = model.pixelMap :: model.history }, Cmd.none )

        UpdateActiveColor color ->
            ( { model | activeColor = color }, Cmd.none )

        PaintPixel color pixelCoords mouseAction ->
            case ( mouseAction, model.mouseIsDown ) of
                ( Clicked, _ ) ->
                    let
                        newPixelMap =
                            Dict.insert pixelCoords color model.pixelMap

                        newHistory =
                            model.pixelMap :: model.history
                    in
                    ( { model
                        | pixelMap = newPixelMap
                        , history = List.take maxHistorySize newHistory
                        , future = []
                      }
                    , Cmd.none
                    )

                ( MouseOver, True ) ->
                    let
                        newPixelMap =
                            Dict.insert pixelCoords color model.pixelMap
                    in
                    ( { model
                        | pixelMap = newPixelMap
                        , future = []
                      }
                    , Cmd.none
                    )

                ( MouseOver, False ) ->
                    ( model, Cmd.none )

        Undo ->
            case model.history of
                currentState :: previousState :: restOfTheList ->
                    let
                        newFuture =
                            currentState :: model.future
                    in
                    ( { model
                        | pixelMap = previousState
                        , history = previousState :: restOfTheList
                        , future = List.take maxFutureSize newFuture
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Redo ->
            case model.future of
                futureState :: restOfTheList ->
                    let
                        newHistory =
                            futureState :: model.history
                    in
                    ( { model
                        | pixelMap = futureState
                        , future = restOfTheList
                        , history = List.take maxHistorySize newHistory
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotMasterpieces result ->
            case result of
                Ok jsonData ->
                    ( { model | fetchState = Success jsonData }, Cmd.none )

                Err _ ->
                    ( { model | fetchState = Error }, Cmd.none )

        None ->
            ( model, Cmd.none )


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


view : Model -> Browser.Document Msg
view model =
    { title = "Fuckin elm"
    , body =
        [ div [ class "container" ]
            [ h1 [] [ text "ART HERO!!!!" ]
            , div [ class "row-bottom" ]
                [ view_color_button model
                , div [ class "column" ]
                    [ div [ class "button-row" ]
                        [ button
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
                            [ text "Redo" ]
                        ]
                    , view_pixel_grid model
                    ]
                ]
            ]
        ]
    }


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



-- API DECODING SHIT --


type alias MasterpieceData =
    Dict PixelCoords Color


type alias MasterpieceList =
    List MasterpieceListItem


type alias MasterpieceListItem =
    { id : Int
    , data : MasterpieceData
    , created_at : String
    }


getMasterpieces : Cmd Msg
getMasterpieces =
    Http.get
        { url = "http://localhost:3000/masterpiece"
        , expect = Http.expectJson GotMasterpieces (D.list masterpieceListItemDecoder)
        }


colourDecoder : D.Decoder Color
colourDecoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "Red" ->
                        D.succeed Red

                    "Black" ->
                        D.succeed Black

                    "Blue" ->
                        D.succeed Blue

                    "Green" ->
                        D.succeed Green

                    "Yellow" ->
                        D.succeed Yellow

                    "White" ->
                        D.succeed White

                    _ ->
                        D.fail ("Unknown colour: " ++ str)
            )


coordsFromString : String -> Result String PixelCoords
coordsFromString str =
    let
        trimmed =
            String.trim str
                |> String.dropLeft 1
                -- remove '['
                |> String.dropRight 1

        -- remove ']'
        parts =
            String.split "," trimmed
    in
    case parts of
        [ xStr, yStr ] ->
            case ( String.toInt xStr, String.toInt yStr ) of
                ( Just x, Just y ) ->
                    Ok ( x, y )

                _ ->
                    Err ("Invalid coordinate numbers in: " ++ str)

        _ ->
            Err ("Invalid coordinate format: " ++ str)


masterpieceDataDecoder : D.Decoder MasterpieceData
masterpieceDataDecoder =
    D.dict colourDecoder
        |> D.andThen
            (\rawDict ->
                let
                    resultList =
                        Dict.toList rawDict
                            |> List.map (\( k, v ) -> Result.map (\coords -> ( coords, v )) (coordsFromString k))
                in
                case
                    List.foldr
                        (\res acc ->
                            case ( res, acc ) of
                                ( Ok ( coords, colour ), Ok dict ) ->
                                    Ok (Dict.insert coords colour dict)

                                ( Err e, _ ) ->
                                    Err e

                                ( _, Err e ) ->
                                    Err e
                        )
                        (Ok Dict.empty)
                        resultList
                of
                    Ok finalDict ->
                        D.succeed finalDict

                    Err msg ->
                        D.fail msg
            )


masterpieceListItemDecoder : D.Decoder MasterpieceListItem
masterpieceListItemDecoder =
    D.map3 MasterpieceListItem
        (D.field "id" D.int)
        (D.field "data" masterpieceDataDecoder)
        (D.field "created_at" D.string)



-- API DECODING SHIT END --
