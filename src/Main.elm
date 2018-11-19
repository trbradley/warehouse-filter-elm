module Main exposing (Model)

import Browser
import Element exposing (..)
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Task



-- INIT


type alias Flags =
    {}


type alias Model =
    { warehouses : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model "", Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Warehouses"
    , body =
        [ Element.layout [ width fill ] <|
            column []
                [ el [ alignTop ] <|
                    Input.button
                        [ Border.color <| rgb255 0 0 0
                        , Border.width 2
                        , Border.rounded 2
                        , padding 5
                        ]
                        { label = text "Load Warehouse CSV"
                        , onPress = Just CsvRequested
                        }
                , el [] <| text model.warehouses
                ]
        ]
    }



-- UPDATE


type Msg
    = CsvRequested
    | CsvLoaded File
    | CsvParsed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvRequested ->
            ( model, requestCsv )

        CsvLoaded file ->
            ( model, loadCsv file )

        CsvParsed warehousesString ->
            ( { model | warehouses = warehousesString }, Cmd.none )


requestCsv : Cmd Msg
requestCsv =
    Select.file [ "text/csv" ] CsvLoaded


loadCsv : File -> Cmd Msg
loadCsv file =
    Task.perform CsvParsed (File.toString file)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
