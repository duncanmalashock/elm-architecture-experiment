module Page exposing (ExternalMessage(..), Model, Msg, init, update, view)

import Html exposing (Html, div)
import TextEditorComponent exposing (ExternalMessage(..))
import Time exposing (Posix)


type ExternalMessage
    = GetCurrentTime (Posix -> Msg)


type alias Model =
    { textEditorA : TextEditorComponent.Model
    , textEditorB : TextEditorComponent.Model
    , status : String
    }


init : Model
init =
    { textEditorA = TextEditorComponent.init "A"
    , textEditorB = TextEditorComponent.init "B"
    , status = ""
    }


type Index
    = A
    | B


type Msg
    = TextEditorComponentMsg Index TextEditorComponent.Msg
    | ReceivedCurrentTimeForTextEditor Index (Posix -> TextEditorComponent.Msg) Posix


update : Model -> Msg -> ( Model, Maybe ExternalMessage )
update model msg =
    case msg of
        ReceivedCurrentTimeForTextEditor index resolveMsg currentTime ->
            case index of
                A ->
                    let
                        ( updatedEditorA, msgFromEditorA ) =
                            TextEditorComponent.update
                                model.textEditorA
                                (resolveMsg currentTime)
                    in
                    ( { model | textEditorA = updatedEditorA }
                    , Nothing
                    )

                B ->
                    let
                        ( updatedEditorB, msgFromEditorB ) =
                            TextEditorComponent.update
                                model.textEditorB
                                (resolveMsg currentTime)
                    in
                    ( { model | textEditorB = updatedEditorB }
                    , Nothing
                    )

        TextEditorComponentMsg index textEditorMsg ->
            case index of
                A ->
                    let
                        ( updatedEditorA, msgFromEditorA ) =
                            TextEditorComponent.update
                                model.textEditorA
                                textEditorMsg

                        ( updatedStatus, externalMsg ) =
                            case msgFromEditorA of
                                Just (ValueAccepted acceptedValue resolveMsg) ->
                                    ( "Editor A value accepted: "
                                        ++ acceptedValue
                                    , Just
                                        (GetCurrentTime
                                            (ReceivedCurrentTimeForTextEditor A
                                                resolveMsg
                                            )
                                        )
                                    )

                                Just (ValueReverted revertedValue) ->
                                    ( "Editor A value reverted: "
                                        ++ revertedValue
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
                    ( { model
                        | textEditorA = updatedEditorA
                        , status = updatedStatus
                      }
                    , externalMsg
                    )

                B ->
                    let
                        ( updatedEditorB, msgFromEditorB ) =
                            TextEditorComponent.update model.textEditorB textEditorMsg

                        ( updatedStatus, externalMsg ) =
                            case msgFromEditorB of
                                Just (ValueAccepted acceptedValue resolveMsg) ->
                                    ( "Editor B value accepted: "
                                        ++ acceptedValue
                                    , Just
                                        (GetCurrentTime
                                            (ReceivedCurrentTimeForTextEditor B
                                                resolveMsg
                                            )
                                        )
                                    )

                                Just (ValueReverted revertedValue) ->
                                    ( "Editor B value reverted: "
                                        ++ revertedValue
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
                    ( { model
                        | textEditorB = updatedEditorB
                        , status = updatedStatus
                      }
                    , externalMsg
                    )


view : Model -> Html Msg
view model =
    div []
        [ TextEditorComponent.view model.textEditorA
            |> Html.map (TextEditorComponentMsg A)
        , Html.br [] []
        , TextEditorComponent.view model.textEditorB
            |> Html.map (TextEditorComponentMsg B)
        , Html.br [] []
        , Html.text model.status
        ]
