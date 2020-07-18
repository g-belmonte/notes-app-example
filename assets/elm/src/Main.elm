module Main exposing (Model, init, Msg, update, view, subscriptions)

import Html exposing (..)
import Http
import Browser
import Browser.Navigation as Nav
import Json.Decode exposing (field)
import Url

import Note exposing (DraftNote, Note, emptyDraftNote, notesDecoder)

apiUrl : String
apiUrl =
    "http://localhost:4000/api"


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
    }

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , notes : Maybe (List Note)
    , draftNote : DraftNote
    }


init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init () url key =
    (Model key url Nothing emptyDraftNote, loadNotes)


loadNotes: Cmd Msg
loadNotes =
    let
        decoder = field "data" notesDecoder
    in
        Http.get
            { url = apiUrl ++ "/notes"
            , expect = Http.expectJson LoadNotes decoder
            }


type Msg
    = LoadNotes (Result Http.Error (List Note))
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadNotes (Ok notes) ->
            ( { model | notes = Just notes }
            , Cmd.none
            )

        -- to do: handle error
        LoadNotes (Err error) ->
            ( model
            , Cmd.none
            )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        [ div []
            [ viewNoteList model.notes ]
      ]
    }


viewNoteList : Maybe (List Note) -> Html Msg
viewNoteList notes =
    case notes of
        Nothing -> text "nothing"
        Just _ -> text "something"
