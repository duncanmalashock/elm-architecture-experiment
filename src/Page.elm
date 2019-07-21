module Page exposing (Config, ExternalMessage(..), Model, Msg, init, update, view)

import Html exposing (Html, div)
import TextEditorComponent exposing (ExternalMessage(..))
import Time exposing (Posix)


type ExternalMessage
    = NeedCurrentTimeForTextEditor (Posix -> Msg)


type alias Config msg =
    { toMsg : Model -> Maybe ExternalMessage -> msg
    , model : Model
    }


type alias Model =
    { textEditorA : TextEditorComponent.Config Msg
    , textEditorB : TextEditorComponent.Config Msg
    , status : String
    }


init : (Model -> Maybe ExternalMessage -> msg) -> Config msg
init toMsg =
    { toMsg = toMsg
    , model =
        { textEditorA = TextEditorComponent.init (UpdatedTextEditorComponent A) "A"
        , textEditorB = TextEditorComponent.init (UpdatedTextEditorComponent B) "B"
        , status = ""
        }
    }


type Index
    = A
    | B


type Msg
    = UpdatedTextEditorComponent Index TextEditorComponent.Model (Maybe TextEditorComponent.ExternalMessage)
    | ReceivedCurrentTimeForTextEditor Index (Posix -> TextEditorComponent.Msg) Posix


update : Config msg -> Msg -> msg
update config msg =
    let
        model =
            config.model
    in
    case msg of
        ReceivedCurrentTimeForTextEditor index resolveMsg currentTime ->
            case index of
                A ->
                    TextEditorComponent.update model.textEditorA (resolveMsg currentTime)
                        |> update config

                B ->
                    TextEditorComponent.update model.textEditorB (resolveMsg currentTime)
                        |> update config

        UpdatedTextEditorComponent index newModel msgFromEditor ->
            case index of
                A ->
                    let
                        textEditorA =
                            model.textEditorA

                        updatedEditorA =
                            { textEditorA | model = newModel }

                        ( updatedStatus, externalMsg ) =
                            case msgFromEditor of
                                Just (ValueAccepted acceptedValue resolveMsg) ->
                                    ( "Editor A value accepted: " ++ acceptedValue
                                    , Just
                                        (NeedCurrentTimeForTextEditor
                                            (ReceivedCurrentTimeForTextEditor A resolveMsg)
                                        )
                                    )

                                Just (ValueReverted revertedValue) ->
                                    ( "Editor A value reverted: " ++ revertedValue
                                    , Nothing
                                    )

                                Just Errored ->
                                    ( "Error triggered in Editor A"
                                    , Nothing
                                    )

                                Nothing ->
                                    ( model.status
                                    , Nothing
                                    )
                    in
                    config.toMsg
                        { model
                            | textEditorA = updatedEditorA
                            , status = updatedStatus
                        }
                        externalMsg

                B ->
                    let
                        textEditorB =
                            model.textEditorB

                        updatedEditorB =
                            { textEditorB | model = newModel }

                        ( updatedStatus, externalMsg ) =
                            case msgFromEditor of
                                Just (ValueAccepted acceptedValue resolveMsg) ->
                                    ( "Editor B value accepted: " ++ acceptedValue
                                    , Just
                                        (NeedCurrentTimeForTextEditor
                                            (ReceivedCurrentTimeForTextEditor B resolveMsg)
                                        )
                                    )

                                Just (ValueReverted revertedValue) ->
                                    ( "Editor B value reverted: " ++ revertedValue
                                    , Nothing
                                    )

                                Just Errored ->
                                    ( "Error triggered in Editor B"
                                    , Nothing
                                    )

                                Nothing ->
                                    ( model.status
                                    , Nothing
                                    )
                    in
                    config.toMsg
                        { model
                            | textEditorB = updatedEditorB
                            , status = updatedStatus
                        }
                        externalMsg


view : Config msg -> Html msg
view ({ model } as config) =
    div []
        [ TextEditorComponent.view model.textEditorA
        , Html.br [] []
        , TextEditorComponent.view model.textEditorB
        , Html.br [] []
        , Html.text model.status
        ]
        |> Html.map (update config)
