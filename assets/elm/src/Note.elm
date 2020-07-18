module Note exposing
    ( Note
    , DraftNote
    , emptyDraftNote
    , noteDecoder
    , notesDecoder
    , updateTitle
    , updateContent
    )

import Json.Decode exposing (Decoder, list, succeed, int, string)
import Json.Decode.Pipeline exposing (required)


type alias Note =
    { id: Int
    , title: String
    , content: String
    }


type alias DraftNote =
    { title: String
    , content: String
    }


noteDecoder : Decoder Note
noteDecoder =
    succeed Note
        |> required "id" int
        |> required "title" string
        |> required "content" string


notesDecoder : Decoder (List Note)
notesDecoder =
    list noteDecoder


updateTitle : String -> Note -> Note
updateTitle newTitle note =
    { note | title = newTitle }


updateContent : String -> Note -> Note
updateContent newContent note =
    { note | content = newContent }


emptyDraftNote : DraftNote
emptyDraftNote =
    DraftNote "" ""
