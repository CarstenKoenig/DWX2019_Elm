module Api.Todos exposing (Url, delete, get, getAll, new, update)

import Http
import Json.Decode as Decode
import Json.Encode as Enc
import Todos


type alias Url =
    String


get : Url -> (Result Http.Error Todos.Item -> msg) -> Todos.Id -> Cmd msg
get baseUrl toMsg todoId =
    Http.get
        { url = baseUrl ++ "todos/" ++ Todos.idToString todoId
        , expect = Http.expectJson toMsg Todos.itemDecoder
        }


getAll : Url -> (Result Http.Error (List Todos.Item) -> msg) -> Cmd msg
getAll baseUrl toMsg =
    Http.get
        { url = baseUrl ++ "todos"
        , expect = Http.expectJson toMsg (Decode.list Todos.itemDecoder)
        }


update : Url -> (Result Http.Error Todos.Item -> msg) -> Todos.Item -> Cmd msg
update baseUrl toMsg todoItem =
    Http.request
        { method = "put"
        , headers = []
        , url = baseUrl ++ "todos"
        , body = Http.jsonBody (Todos.encodeItem todoItem)
        , expect = Http.expectJson toMsg Todos.itemDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


delete : Url -> (Result Http.Error (List Todos.Item) -> msg) -> Todos.Id -> Cmd msg
delete baseUrl toMsg todoId =
    Http.request
        { method = "delete"
        , headers = []
        , url = baseUrl ++ "todos/" ++ Todos.idToString todoId
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (Decode.list Todos.itemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


new : Url -> (Result Http.Error Todos.Item -> msg) -> String -> Cmd msg
new baseUrl toMsg text =
    Http.post
        { url = baseUrl ++ "todos"
        , body = Http.jsonBody (Enc.string text)
        , expect = Http.expectJson toMsg Todos.itemDecoder
        }
