module Todos exposing
    ( Id
    , Item
    , Todos
    , activeTodos
    , allCompleted
    , allTodos
    , completedTodos
    , deleteItem
    , empty
    , encodeItem
    , fromList
    , get
    , idToString
    , insertItem
    , isEmpty
    , itemDecoder
    , setCompleted
    , setText
    , toggle
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Todos
    = Todos
        { dict : Dict Int Item
        }


type alias Item =
    { id : Id
    , text : String
    , completed : Bool
    }


itemDecoder : Decoder Item
itemDecoder =
    Decode.map3
        Item
        (Decode.field "id" idDecoder)
        (Decode.field "text" Decode.string)
        (Decode.field "finished" Decode.bool)


encodeItem : Item -> Value
encodeItem item =
    Encode.object
        [ ( "id", encodeId item.id )
        , ( "text", Encode.string item.text )
        , ( "finished", Encode.bool item.completed )
        ]


type Id
    = Id Int


idDecoder : Decoder Id
idDecoder =
    Decode.map Id Decode.int


encodeId : Id -> Value
encodeId (Id id) =
    Encode.int id


idToString : Id -> String
idToString (Id id) =
    String.fromInt id


idToInt : Id -> Int
idToInt (Id id) =
    id


empty : Todos
empty =
    Todos { dict = Dict.empty }


isEmpty : Todos -> Bool
isEmpty (Todos todos) =
    Dict.isEmpty todos.dict


allCompleted : Todos -> Bool
allCompleted todos =
    List.length (allTodos todos) == List.length (completedTodos todos)


get : Id -> Todos -> Maybe Item
get (Id id) (Todos todos) =
    Dict.get id todos.dict


fromList : List Item -> Todos
fromList items =
    let
        dict =
            items
                |> List.map (\item -> ( idToInt item.id, item ))
                |> Dict.fromList
    in
    Todos { dict = dict }


allTodos : Todos -> List Item
allTodos =
    toList


activeTodos : Todos -> List Item
activeTodos =
    List.filter (\item -> item.completed == False) << toList


completedTodos : Todos -> List Item
completedTodos =
    List.filter (\item -> item.completed == True) << toList


toList : Todos -> List Item
toList (Todos todos) =
    Dict.values todos.dict


insertItem : Item -> Todos -> Todos
insertItem item (Todos todos) =
    Todos { todos | dict = Dict.insert (idToInt item.id) item todos.dict }


deleteItem : Id -> Todos -> Todos
deleteItem id =
    updateItem (always Nothing) id
        >> Tuple.second


setCompleted : Bool -> Id -> Todos -> ( Maybe Item, Todos )
setCompleted completed =
    updateItem (Maybe.map (\item -> { item | completed = completed }))


toggle : Id -> Todos -> ( Maybe Item, Todos )
toggle =
    updateItem (Maybe.map (\item -> { item | completed = not item.completed }))


setText : String -> Id -> Todos -> ( Maybe Item, Todos )
setText text =
    updateItem (Maybe.map (\item -> { item | text = text }))


updateItem : (Maybe Item -> Maybe Item) -> Id -> Todos -> ( Maybe Item, Todos )
updateItem update (Id id) (Todos todos) =
    let
        foundItem =
            Dict.get id todos.dict

        updatedItem =
            update foundItem

        newDict =
            case updatedItem of
                Nothing ->
                    Dict.remove id todos.dict

                Just item ->
                    Dict.insert id item todos.dict
    in
    ( updatedItem, Todos { todos | dict = newDict } )
