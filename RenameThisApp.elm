module RenameThisApp exposing (init, main, update, view)

{-| This is generic doc explaining what the hell this is.

    Well, it's a blank app template that does no real work.

    A convenience for beginners like me who don't care for
    the more heavy-handed/opinionated project generators. :)


# Program Functions

@docs init, main, update, view

-}

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (..)


-- MODEL


type alias Flags =
    { incomingRecords : List Int
    , someData : String
    }


type alias Model =
    { flagRecords : List Int
    , textFieldValue : String
    , col1Value : String
    , toggableState : Bool
    , displayedRecords : List Int
    }



-- UPDATE


type Msg
    = KeyUp Int
    | FieldChanged String
    | ToggleSomeState
    | ButtonPressed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyUp key ->
            handleKeyUp key model

        FieldChanged fieldValue ->
            handleFieldChanged fieldValue model

        ToggleSomeState ->
            handleToggleSomeState model

        ButtonPressed ->
            handleButtonPressed model


handleKeyUp : Int -> Model -> ( Model, Cmd Msg )
handleKeyUp key model =
    if key == 13 then
        ( { model
            | col1Value = model.textFieldValue
            , textFieldValue = ""
          }
        , Cmd.none
        )
    else
        ( model, Cmd.none )


handleFieldChanged : String -> Model -> ( Model, Cmd Msg )
handleFieldChanged fieldValue model =
    ( { model | textFieldValue = fieldValue }, Cmd.none )


handleToggleSomeState : Model -> ( Model, Cmd Msg )
handleToggleSomeState model =
    ( { model | toggableState = not model.toggableState }, Cmd.none )


handleButtonPressed : Model -> ( Model, Cmd Msg )
handleButtonPressed model =
    if List.length model.displayedRecords == 0 then
        ( { model | displayedRecords = List.map someTransform model.flagRecords }, Cmd.none )
    else
        ( { model | displayedRecords = [] }, Cmd.none )


someTransform : Int -> Int
someTransform value =
    value * 2



-- VIEW


view : Model -> Html Msg
view model =
    let
        toggledClass =
            if model.toggableState then
                "row blinking"
            else
                "row"

        recordCount =
            List.length model.displayedRecords
    in
        div []
            [ div
                [ class "row" ]
                [ div [ class "col w200" ]
                    [ input
                        [ type_ "text"
                        , onInput FieldChanged
                        , onKeyUp KeyUp
                        , Attr.value model.textFieldValue
                        ]
                        []
                    ]
                , div [ class "col w100" ]
                    [ input
                        [ type_ "checkbox"
                        , onClick ToggleSomeState
                        ]
                        []
                    ]
                , div [ class "col w100" ]
                    [ input
                        [ type_ "button"
                        , Attr.value "Press Me"
                        , onClick ButtonPressed
                        ]
                        []
                    ]
                , div [ class "col " ] []
                ]
            , div
                [ class toggledClass ]
                [ div [ class "col w200" ] [ text model.col1Value ]
                , div [ class "col w100" ] [ text "col2" ]
                , div [ class "col w100" ] [ text (toString model.displayedRecords) ]
                , div [ class "col " ] [ text (toString recordCount) ]
                ]
            ]


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)



--  MAIN


initialModel : Model
initialModel =
    { flagRecords = []
    , textFieldValue = "Some Value"
    , col1Value = "I'm a target!"
    , toggableState = False
    , displayedRecords = []
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { initialModel | flagRecords = flags.incomingRecords }, Cmd.none )


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
