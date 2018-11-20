module Main exposing (Model)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Json.Decode exposing (Decoder, field, map2, string)
import Task



-- INIT


type alias Flags =
    {}


type alias Model =
    { warehouses : Warehouse }


type alias Warehouse =
    { id : String
    , name : String

    -- , location : String
    -- , lat : Float
    -- , lon : Float
    -- , rating : Float
    -- , temperature : String
    -- , capacity : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { warehouses = { id = "", name = "" } }
    in
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ width fill ] <|
        column []
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
            , el [] <| text model.warehouses.name
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


requestJson : Cmd Msg
requestJson =
    Select.file [ "application/json" ] JsonLoaded


loadJson : File -> Cmd Msg
loadJson json =
    Task.perform JsonParsed <| File.toString json


decodeJson : String -> Warehouse
decodeJson warehouses =
    case Json.Decode.decodeString decodeWarehouse warehouses of
        Ok result ->
            result

        Err error ->
            { id = "", name = "" }


decodeWarehouse : Decoder Warehouse
decodeWarehouse =
    map2 Warehouse
        (field "id" string)
        (field "name" string)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
