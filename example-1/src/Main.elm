module Main exposing (main)

import Browser
import Page exposing (ExternalMessage(..))
import Task
import Time


type alias Model =
    { page : Page.Config Msg
    }


type Msg
    = UpdatedPage Page.Model (Maybe Page.ExternalMessage)


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
    ( { page = Page.init UpdatedPage
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Sample app"
    , body =
        [ Page.view model.page
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedPage newPageModel msgFromPage ->
            let
                pageConfig =
                    model.page

                updatedPageConfig =
                    { pageConfig | model = newPageModel }

                getCurrentTimeCmd =
                    case msgFromPage of
                        Just (NeedCurrentTimeForTextEditor resolveMsg) ->
                            Time.now
                                |> Task.perform
                                    (resolveMsg >> Page.update updatedPageConfig)

                        Nothing ->
                            Cmd.none
            in
            ( { model | page = updatedPageConfig }
            , getCurrentTimeCmd
            )
