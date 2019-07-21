module TextEditorComponent exposing (ExternalMessage(..), Model, Msg, init, update, view)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Time exposing (Posix)


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
    = ValueAccepted String (Posix -> Msg)
    | ValueReverted String
    | Errored


update : Model -> Msg -> ( Model, Maybe ExternalMessage )
update model msg =
    case msg of
        UpdateValue string ->
            ( { model | value = string }
            , Nothing
            )

        UpdateAcceptedAt posix ->
            ( { model | acceptedAt = Just posix }
            , Nothing
            )

        Revert ->
            ( { model | value = model.revertValue }
            , Just (ValueReverted model.revertValue)
            )

        Accept ->
            ( { model | revertValue = model.value }
            , Just (ValueAccepted model.value UpdateAcceptedAt)
            )

        Error ->
            ( model
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
