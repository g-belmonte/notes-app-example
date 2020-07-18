module Main exposing (Model, init, Msg, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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
    | DeleteNote Int
    | DeletedNote (Result Http.Error ())
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


        DeletedNote _ ->
            ( model
            , Cmd.none
            )


        DeleteNote id ->
            ( model
            , Http.request
                { method = "DELETE"
                , headers = []
                , url = apiUrl ++ "/notes/" ++ String.fromInt id
                , body = Http.emptyBody
                , expect = Http.expectWhatever DeletedNote
                , timeout = Nothing
                , tracker = Nothing
                }
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
        [ div [ class "row" ]
              [ div [ class "col s12 l8 offset-l2" ]
                    [ viewNoteList model.notes ]
              ]
        ]
    }


viewNoteList : Maybe (List Note) -> Html Msg
viewNoteList notes =
    case notes of
        Nothing ->
            h5 [ class "center-align" ] [ text "Loading" ]

        Just [] ->
            h5 [ class "center-align" ] [ text "There's nothing around here." ]

        Just noteList ->
            div [ class "row" ] (List.map viewNote noteList)


viewNote : Note -> Html Msg
viewNote note =
    div [ class "col s6 m4 l3" ]
        [ div [ class "card small grey lighten-2 hoverable" ]
              [ div [ class "card-content" ]
                    [ span [ class "card-title center-align" ] [ text note.title ]
                    , p [ class "card-content" ] [ text note.content ]
                    ]
              , div [class "card-action"]
                  [ div [ class "btn-flat btn-small waves-effect waves-light waves-grey right", onClick (DeleteNote note.id) ]
                        [ i [ class "material-icons small grey-text text-darken-2" ] [ text "delete" ] ]
                  , div [ class "btn-flat btn-small waves-effect waves-light waves-grey right" ]
                        [ i [ class "material-icons small grey-text text-darken-2" ] [ text "edit" ] ]
                  ]
              ]
        ]
