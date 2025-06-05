module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, input, label, p, text)
import Html.Attributes exposing (class, placeholder, style, value)
import Html.Events exposing (onClick, onInput, onMouseDown, onMouseOver, onMouseUp)
import Http
import Json.Decode as D
import Json.Encode as E
import Url exposing (Protocol(..))


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

type BrushSize 
    = Small 
    | Medium
    | Large


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
    , name : String
    , brushSize : BrushSize
    }


init : Bool -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { mouseIsDown = False
      , activeColor = Red
      , pixelMap = Dict.empty
      , history = [ Dict.empty ]
      , future = [ Dict.empty ]
      , fetchState = Loading
      , name = ""
      , brushSize = Small 
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
    | UpdateBrushSize BrushSize
    | PaintPixel Color PixelCoords MouseAction
    | Undo
    | Redo
    | GotMasterpieces (Result Http.Error FetchedMasterpieces)
    | SaveMasterPiece
    | SavedMasterPiece (Result Http.Error MasterpieceListItem)
    | UpdateName String
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

        UpdateBrushSize newBrushSize ->
            ({ model | brushSize = newBrushSize }, Cmd.none)


        PaintPixel color pixelCoords mouseAction ->
            case ( mouseAction, model.mouseIsDown ) of
                ( Clicked, _ ) ->
                    let
                        newPixelMap =
                            paintAtBrushSize
                                model.brushSize
                                pixelCoords
                                color
                                model.pixelMap   

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
                            paintAtBrushSize 
                                model.brushSize
                                pixelCoords
                                color
                                model.pixelMap  
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

        UpdateName newName ->
            ( { model | name = newName }, Cmd.none )

        GotMasterpieces result ->
            case result of
                Ok jsonData ->
                    ( { model | fetchState = Success jsonData }, Cmd.none )

                Err _ ->
                    ( { model | fetchState = Error }, Cmd.none )

        SaveMasterPiece ->
            ( model, saveMasterpiece model.name model.pixelMap )

        SavedMasterPiece result ->
            case result of
                Ok masterpiece ->
                    -- If fetchStat is failed, then we set it to success, with the one saved masterpiece
                    -- if it is success, we append the saved masterpiece to state
                    case model.fetchState of
                        Success existingMasterpieces ->
                            ( { model | fetchState = Success (masterpiece :: existingMasterpieces) }, Cmd.none )

                        _ ->
                            ( { model | fetchState = Success [ masterpiece ] }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

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

paintAtBrushSize : BrushSize
    -> PixelCoords
    -> Color
    -> Dict PixelCoords Color
    -> Dict PixelCoords Color

paintAtBrushSize brushSize pixelCoords color pixelMap =
    case brushSize of
        Small ->
            Dict.insert pixelCoords color pixelMap

        Medium ->
            let
                ( x, y ) = pixelCoords
                coordsToPaint =
                    [(x, y), ( x - 1, y - 1 ), ( x - 1, y ), ( x - 1, y+1 )
                    , ( x, y - 1 ), ( x, y + 1 )
                    , ( x + 1, y - 1 ), ( x + 1, y ), ( x + 1, y + 1 )
                    ]
            in
            List.foldl (\coord acc -> Dict.insert coord color acc) pixelMap coordsToPaint

        Large ->
            let
                (x, y) = pixelCoords
                coordsToPaint =
                    [(x, y), ( x - 1, y - 1 ), ( x - 1, y ), ( x - 1, y+1 ), ( x - 1, y+2 ), (x - 1, y - 2)
                    , ( x - 2, y - 2 ), ( x - 2, y - 1 ), ( x - 2, y ), ( x - 2, y+1 ), ( x - 2, y+2 )
                    , ( x, y - 1 ), ( x, y + 1 ), (x, y - 2), (x, y + 2)
                    , ( x+1, y - 2 ), ( x+1, y - 1 ), ( x+1, y ), ( x+1, y+1 ), ( x+1, y+2 )
                    , ( x+2, y), ( x+2, y - 1 ), ( x+2, y+2 ), ( x+2, y - 2 ), ( x+2, y+1)
                    ]
            in
            List.foldl (\coord acc -> Dict.insert coord color acc) pixelMap coordsToPaint



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
        ([ p [] [ text "Get yer colours!" ] ]
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

view_brush_size_button : Model -> Html Msg
view_brush_size_button model =
    div
    [ class "brush-size-picker"
    ]
     ([ p [] [ text "Get yer brush stroke size!" ] ]
        ++ List.map
            (\brushSize ->
                button
                    [ onClick (UpdateBrushSize brushSize)
                    , class
                        (if model.brushSize == brushSize then
                            "brush-size-button active-button"

                            else
                            "brush-size-button"
                        )
                    ]
                    [ text (case brushSize of
                        Small ->
                            "Small"

                        Medium ->
                            "Medium"

                        Large ->
                            "Large"
                    )]
            )
            [ Small, Medium, Large]
     )

view : Model -> Document Msg
view model =
    { title = "Fuckin elm"
    , body =
        [ div [ class "container" ]
            [ h1 [] [ text "ART HERO!!!!" ]
            , div [ class "row-bottom" ]
                [ view_color_button model
                   ,view_brush_size_button model
                , div [ class "column" ]
                    [ div
                        [ class "input-row"
                        ]
                        [ label [] [ text "Name:" ]
                        , input [ placeholder "Mona lisa", value model.name, onInput UpdateName ] []
                        ]
                    , div [ class "button-row" ]
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
                        , button [ onClick SaveMasterPiece, class "button" ] [ text "Save Masterpiece!" ]
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
    , pixelMap : MasterpieceData
    , name : String
    , created_at : String
    }


getMasterpieces : Cmd Msg
getMasterpieces =
    Http.get
        { url = "http://localhost:3000/masterpiece"
        , expect = Http.expectJson GotMasterpieces (D.list masterpieceListItemDecoder)
        }


encodeColor : Color -> E.Value
encodeColor color =
    case color of
        Red ->
            E.string "Red"

        Black ->
            E.string "Black"

        Blue ->
            E.string "Blue"

        Green ->
            E.string "Green"

        Yellow ->
            E.string "Yellow"

        White ->
            E.string "White"


encodePixelCoords : PixelCoords -> String
encodePixelCoords ( x, y ) =
    "[" ++ String.fromInt x ++ "," ++ String.fromInt y ++ "]"


encodePixelMap : PixelMap -> E.Value
encodePixelMap pixelMap =
    Dict.toList pixelMap
        |> List.map (\( coords, color ) -> ( encodePixelCoords coords, encodeColor color ))
        |> E.object


encodeMasterpiece : String -> PixelMap -> E.Value
encodeMasterpiece name pixelMap =
    E.object
        [ ( "name", E.string name )
        , ( "pixelMap", encodePixelMap pixelMap )
        ]


saveMasterpiece : String -> PixelMap -> Cmd Msg
saveMasterpiece name pixelMap =
    Http.post
        { url = "http://localhost:3000/masterpiece"
        , expect = Http.expectJson SavedMasterPiece masterpieceListItemDecoder
        , body = Http.jsonBody (encodeMasterpiece name pixelMap)
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



masterpieceDataFieldDecoder : D.Decoder ( MasterpieceData, String )
masterpieceDataFieldDecoder =
    D.map2 Tuple.pair
        (D.field "pixelMap" masterpieceDataDecoder)
        (D.field "name" D.string)


masterpieceListItemDecoder : D.Decoder MasterpieceListItem
masterpieceListItemDecoder =
    D.map3
        (\id ( pixelMap, name ) created_at ->
            { id = id
            , pixelMap = pixelMap
            , name = name
            , created_at = created_at
            }
        )
        (D.field "id" D.int)
        (D.field "data" masterpieceDataFieldDecoder)
        (D.field "created_at" D.string)



-- API DECODING SHIT END --
