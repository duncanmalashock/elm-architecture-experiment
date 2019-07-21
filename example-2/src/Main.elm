module Main exposing (Msg, main)

import Browser
import Html
import Page exposing (ExternalMessage(..))
import Task
import Time


type alias Model =
    { page : Page.Model
    }


type Msg
    = PageMsg Page.Msg


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = Page.init
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Sample app"
    , body =
        [ Page.view model.page
            |> Html.map PageMsg
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageMsg pageMsg ->
            let
                ( updatedPage, msgFromPage ) =
                    Page.update model.page pageMsg

                getCurrentTimeCmd =
                    case msgFromPage of
                        Just (GetCurrentTime resolveMsg) ->
                            Time.now
                                |> Task.perform
                                    (resolveMsg >> PageMsg)

                        Nothing ->
                            Cmd.none
            in
            ( { model | page = updatedPage }
            , getCurrentTimeCmd
            )
