module Main exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser
import Browser.Navigation as Nav
import Channel
import Html exposing (..)
import Html.Attributes exposing (class, for, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (decodeValue, field, int, string)
import Json.Encode as E
import Note exposing (DraftNote, Note, emptyDraftNote, noteDecoder, notesDecoder, updateDraftContent, updateDraftTitle)
import Ports.Phoenix as Phx
import Socket
import Url


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


type alias NoteId =
    Int


type NoteState
    = NoteList
    | NoteEdit NoteId
    | NoteNew


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , notes : Maybe (List Note)
    , draftNote : DraftNote
    , state : NoteState
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    ( Model key url Nothing emptyDraftNote NoteList, loadNotes )


loadNotes : Cmd Msg
loadNotes =
    let
        decoder =
            field "data" notesDecoder
    in
    Http.get
        { url = apiUrl ++ "/notes"
        , expect = Http.expectJson LoadNotes decoder
        }


type Msg
    = LoadNotes (Result Http.Error (List Note))
    | SocketMsg Socket.EventIn
    | ChannelMsg Channel.EventIn
    | DeleteNote NoteId
    | EditNote NoteId
    | EditTitle String
    | EditContent String
    | EditCancel
    | EditSave NoteId
    | NoOp (Result Http.Error ())
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadNotes (Ok notes) ->
            ( { model | notes = Just notes }
            , Socket.send (Socket.Connect Nothing) Phx.sendMessage
            )

        -- to do: handle error
        LoadNotes (Err error) ->
            ( model
            , Cmd.none
            )

        SocketMsg Socket.Opened ->
            ( model
            , Channel.send
                (Channel.Join
                    { topic = "notes:lobby"
                    , timeout = Nothing
                    , payload = Nothing
                    }
                )
                Phx.sendMessage
            )

        ChannelMsg (Channel.JoinOk "notes:lobby" payload) ->
            ( model
            , Channel.eventsOn
                (Just "notes:lobby")
                [ "delete"
                , "update"
                ]
                Phx.sendMessage
            )

        ChannelMsg (Channel.Message "notes:lobby" "delete" payload) ->
            let
                key =
                    decodeValue (field "id" int) payload
            in
            case key of
                Ok id ->
                    ( { model
                        | notes = Maybe.map (List.filter (\x -> x.id /= id)) model.notes
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ChannelMsg (Channel.Message "notes:lobby" "update" payload) ->
            let
                newNote =
                    Result.withDefault (Note 0 "" "") <| decodeValue noteDecoder payload

                noteList =
                    List.filter (\x -> x.id /= newNote.id) <| Maybe.withDefault [] model.notes
            in
            ( { model
                | notes = Just (newNote :: noteList)
              }
            , Cmd.none
            )

        DeleteNote id ->
            ( model
            , Http.request
                { method = "DELETE"
                , headers = []
                , url = apiUrl ++ "/notes/" ++ String.fromInt id
                , body = Http.emptyBody
                , expect = Http.expectWhatever NoOp
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        EditNote id ->
            let
                note =
                    Maybe.withDefault (Note 0 "" "") <| (List.head << List.filter (\n -> n.id == id)) <| Maybe.withDefault [] model.notes
            in
            ( { model
                | state = NoteEdit note.id
                , draftNote = DraftNote note.title note.content
              }
            , Cmd.none
            )

        EditTitle newTitle ->
            ( { model
                | draftNote = updateDraftTitle newTitle model.draftNote
              }
            , Cmd.none
            )

        EditContent newContent ->
            ( { model
                | draftNote = updateDraftContent newContent model.draftNote
              }
            , Cmd.none
            )

        EditCancel ->
            ( { model
                | draftNote = emptyDraftNote
                , state = NoteList
              }
            , Cmd.none
            )

        EditSave id ->
            ( { model
                | draftNote = emptyDraftNote
                , state = NoteList
              }
            , Http.request
                { method = "PATCH"
                , headers = []
                , url = apiUrl ++ "/notes/" ++ String.fromInt id
                , body =
                    Http.jsonBody
                        (E.object
                            [ ( "note"
                              , E.object
                                    [ ( "title", E.string model.draftNote.title )
                                    , ( "content", E.string model.draftNote.content )
                                    ]
                              )
                            ]
                        )
                , expect = Http.expectWhatever NoOp
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

        {- Catch Alls -}
        SocketMsg _ ->
            ( model, Cmd.none )

        ChannelMsg _ ->
            ( model, Cmd.none )

        NoOp _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Phx.socketReceiver
            |> Socket.subscriptions
                SocketMsg
        , Phx.channelReceiver
            |> Channel.subscriptions
                ChannelMsg
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        [ div [ class "row" ]
            [ div [ class "col s12 l8 offset-l2" ]
                [ case model.state of
                    NoteList ->
                        viewNoteList model.notes

                    NoteEdit id ->
                        viewNoteEdit id model.draftNote

                    NoteNew ->
                        Debug.todo "new note"
                ]
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
            , div [ class "card-action" ]
                [ div [ class "btn-flat btn-small waves-effect waves-light waves-grey right", onClick (DeleteNote note.id) ]
                    [ i [ class "material-icons small grey-text text-darken-2" ] [ text "delete" ] ]
                , div [ class "btn-flat btn-small waves-effect waves-light waves-grey right", onClick (EditNote note.id) ]
                    [ i [ class "material-icons small grey-text text-darken-2" ] [ text "edit" ] ]
                ]
            ]
        ]


viewNoteEdit : NoteId -> DraftNote -> Html Msg
viewNoteEdit noteId draft =
    div [ class "col s12 m10 offset-m2" ]
        [ div [ class "row" ]
            [ div [ class "input-field col s10 m8 offset-s1 offset-m1" ]
                [ input [ id "note-title", type_ "text", value draft.title, onInput EditTitle ] []
                , label [ for "note-title" ] [ text "Title" ]
                ]
            , div [ class "input-field col s10 m8 offset-s1 offset-m1" ]
                [ textarea [ id "note-content", class "materialize-textarea", value draft.content, onInput EditContent ] []
                , label [ for "note-content" ] [ text "Content" ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "btn-flat btn-small waves-effect waves-light waves-grey right", onClick (EditSave noteId) ] [ text "Save" ]
            , div [ class "btn-flat btn-small waves-effect waves-light waves-grey right", onClick EditCancel ] [ text "Cancel" ]
            ]
        ]
