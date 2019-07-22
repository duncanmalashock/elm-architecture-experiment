module TextEditorComponent exposing (Config, ExternalMessage(..), Model, Msg, init, update, view)

import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Time exposing (Posix)


type alias Config msg =
    { toMsg : Model -> Maybe ExternalMessage -> msg
    , model : Model
    }


type alias Model =
    { value : String
    , revertValue : String
    , acceptedAt : Maybe Posix
    }


init : (Model -> Maybe ExternalMessage -> msg) -> String -> Config msg
init toMsg value =
    { toMsg = toMsg
    , model =
        { value = value
        , revertValue = value
        , acceptedAt = Nothing
        }
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


update : Config msg -> Msg -> msg
update config msg =
    let
        model =
            config.model
    in
    case msg of
        UpdateValue string ->
            config.toMsg
                { model | value = string }
                Nothing

        UpdateAcceptedAt posix ->
            config.toMsg
                { model | acceptedAt = Just posix }
                Nothing

        Revert ->
            config.toMsg
                { model | value = model.revertValue }
                (Just <| ValueReverted model.revertValue)

        Accept ->
            config.toMsg
                { model | revertValue = model.value }
                (Just (ValueAccepted model.value UpdateAcceptedAt))

        Error ->
            config.toMsg
                model
                (Just Errored)


view : Config msg -> Html msg
view ({ model } as config) =
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
        |> Html.map (update config)
