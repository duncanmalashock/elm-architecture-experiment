module Page exposing (ExternalMessage(..), Model, Msg, init, update, view)

import Html exposing (Html, div)
import TextEditorComponent exposing (ExternalMessage(..))


type ExternalMessage
    = NoOp


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


update : Model -> Msg -> ( Model, Cmd Msg, Maybe ExternalMessage )
update model msg =
    case msg of
        TextEditorComponentMsg index textEditorMsg ->
            case index of
                A ->
                    let
                        ( updatedEditorA, editorCmd, msgFromEditor ) =
                            TextEditorComponent.update
                                model.textEditorA
                                textEditorMsg

                        ( updatedStatus, cmd, externalMsg ) =
                            case msgFromEditor of
                                Just (ValueAccepted acceptedValue) ->
                                    ( "Editor A value accepted: "
                                        ++ acceptedValue
                                    , Cmd.map (TextEditorComponentMsg A) editorCmd
                                    , Nothing
                                    )

                                Just (ValueReverted revertedValue) ->
                                    ( "Editor A value reverted: "
                                        ++ revertedValue
                                    , Cmd.none
                                    , Nothing
                                    )

                                Just Errored ->
                                    ( "Error triggered in Editor A"
                                    , Cmd.none
                                    , Nothing
                                    )

                                Nothing ->
                                    ( model.status
                                    , Cmd.none
                                    , Nothing
                                    )
                    in
                    ( { model
                        | textEditorA = updatedEditorA
                        , status = updatedStatus
                      }
                    , cmd
                    , externalMsg
                    )

                B ->
                    let
                        ( updatedEditorB, editorCmd, msgFromEditor ) =
                            TextEditorComponent.update
                                model.textEditorB
                                textEditorMsg

                        ( updatedStatus, cmd, externalMsg ) =
                            case msgFromEditor of
                                Just (ValueAccepted acceptedValue) ->
                                    ( "Editor B value accepted: "
                                        ++ acceptedValue
                                    , Cmd.map (TextEditorComponentMsg B) editorCmd
                                    , Nothing
                                    )

                                Just (ValueReverted revertedValue) ->
                                    ( "Editor B value reverted: "
                                        ++ revertedValue
                                    , Cmd.none
                                    , Nothing
                                    )

                                Just Errored ->
                                    ( "Error triggered in Editor B"
                                    , Cmd.none
                                    , Nothing
                                    )

                                Nothing ->
                                    ( model.status
                                    , Cmd.none
                                    , Nothing
                                    )
                    in
                    ( { model
                        | textEditorB = updatedEditorB
                        , status = updatedStatus
                      }
                    , cmd
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
