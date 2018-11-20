module Main exposing (Model)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Json.Decode exposing (Decoder, float, int, string)
import Json.Decode.Pipeline exposing (required)
import Task



-- INIT


type alias Flags =
    {}


type alias Model =
    { warehouses : List Warehouse }


type alias Warehouse =
    { id : String
    , name : String
    , location : String
    , lat : Float
    , lon : Float
    , rating : Float
    , temperature : String
    , capacity : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { warehouses = [] }, Cmd.none )



-- VIEW


edges =
    { top = 0, right = 0, bottom = 0, left = 0 }


view : Model -> Html Msg
view model =
    Element.layout [ width fill ] <|
        column [ width fill ]
            [ el [ alignTop ] <|
                Input.button
                    [ Border.color <| rgb255 0 0 0
                    , Border.width 2
                    , Border.rounded 2
                    , padding 5
                    ]
                    { label = text "Load Warehouse JSON"
                    , onPress = Just JsonRequested
                    }
            , row [ width fill, centerX, paddingXY 0 20, Font.bold, Font.size 30 ] [ text "Warehouses" ]
            , column [ width fill, spacing 10 ] <| List.map warehouseRow model.warehouses
            ]


warehouseRow : Warehouse -> Element Msg
warehouseRow warehouse =
    row [ width fill, centerX, spacing 20, paddingEach { edges | bottom = 5 }, Border.widthEach { edges | bottom = 1 }, Border.color <| rgb 0 0 0 ]
        [ el [ width <| fillPortion 1 ] <| text warehouse.id
        , el [ width <| fillPortion 2 ] <| text warehouse.name
        , el [ width <| fillPortion 1 ] <| text warehouse.location
        , el [ width <| fillPortion 1 ] <| text <| String.fromFloat warehouse.lat
        , el [ width <| fillPortion 1 ] <| text <| String.fromFloat warehouse.lon
        , el [ width <| fillPortion 1 ] <| text <| String.fromFloat warehouse.rating
        , el [ width <| fillPortion 1 ] <| text warehouse.temperature
        , el [ width <| fillPortion 1 ] <| text <| String.fromInt warehouse.capacity
        ]



-- UPDATE


type Msg
    = JsonRequested
    | JsonLoaded File
    | JsonParsed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JsonRequested ->
            ( model, requestJson )

        JsonLoaded json ->
            ( model, loadJson json )

        JsonParsed warehouses ->
            ( { model | warehouses = decodeJson warehouses }, Cmd.none )



--DECODING


requestJson : Cmd Msg
requestJson =
    Select.file [ "application/json" ] JsonLoaded


loadJson : File -> Cmd Msg
loadJson json =
    Task.perform JsonParsed <| File.toString json


decodeJson : String -> List Warehouse
decodeJson warehouses =
    case Json.Decode.decodeString warehousesDecoder warehouses of
        Ok result ->
            result

        Err error ->
            []


warehousesDecoder : Decoder (List Warehouse)
warehousesDecoder =
    Json.Decode.list warehouseDecoder


warehouseDecoder : Decoder Warehouse
warehouseDecoder =
    Json.Decode.succeed Warehouse
        |> required "id" string
        |> required "name" string
        |> required "location" string
        |> required "lat" float
        |> required "lon" float
        |> required "rating" float
        |> required "temperature" string
        |> required "capacity_sq_ft" int



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
