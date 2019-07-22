module TextEditorComponent exposing (ExternalMessage(..), Model, Msg, init, update, view)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Time exposing (Posix)
import Task

type alias Model =
    { value : String
    , revertValue : String
    , acceptedAt : Maybe Posix
    }


init : String -> Model
init value =
    { value = value
    , revertValue = value
    , acceptedAt = Nothing
    }


type Msg
    = UpdateValue String
    | UpdateAcceptedAt Posix
    | Revert
    | Accept
    | Error


type ExternalMessage
    = ValueAccepted String
    | ValueReverted String
    | Errored


update : Model -> Msg -> ( Model, Cmd Msg, Maybe ExternalMessage )
update model msg =
    case msg of
        UpdateValue string ->
            ( { model | value = string }
            , Cmd.none
            , Nothing
            )

        UpdateAcceptedAt posix ->
            ( { model | acceptedAt = Just posix }
            , Cmd.none
            , Nothing
            )

        Revert ->
            ( { model | value = model.revertValue }
            , Cmd.none
            , Just <| ValueReverted model.revertValue
            )

        Accept ->
            ( { model | revertValue = model.value }
            , Time.now
                |> Task.perform UpdateAcceptedAt
            , Just <| ValueAccepted model.value
            )

        Error ->
            ( model
            , Cmd.none
            , Just Errored
            )


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput UpdateValue, value model.value ] []
        , button
            [ onClick Accept
            , Html.Attributes.disabled (model.value == model.revertValue)
            ]
            [ text "Accept" ]
        , button
            [ onClick Revert
            , Html.Attributes.disabled (model.value == model.revertValue)
            ]
            [ text "Revert" ]
        , button [ onClick Error ]
            [ text "(Error Test)" ]
        ]
