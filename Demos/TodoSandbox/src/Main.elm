module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Json.Decode as Decode
import Todos exposing (Todos)


main : Program () Model Message
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


type alias Model =
    { todos : Todos
    , newText : String
    , editingItem : Maybe { id : Todos.Id, text : String }
    }


type Message
    = NoOp
    | UpdateNewText String
    | ClearNewText
    | AddTodo
    | CheckItem Todos.Id Bool
    | DeleteItem Todos.Id
    | EditItem Todos.Item
    | UpdateEditText String
    | FinishEdit
    | CancelEdit
    | ClearCompleted
    | ToggleAll Bool


initialModel : Model
initialModel =
    { todos = Todos.empty
    , newText = ""
    , editingItem = Nothing
    }


update : Message -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        UpdateNewText updatedText ->
            { model | newText = updatedText }

        ClearNewText ->
            { model | newText = "" }

        AddTodo ->
            let
                ( _, newTodos ) =
                    Todos.addNew model.newText model.todos
            in
            { model | todos = newTodos, newText = "" }

        CheckItem itemId completed ->
            let
                ( newItem, newTodos ) =
                    Todos.setCompleted completed itemId model.todos
            in
            { model | todos = newTodos }

        DeleteItem itemId ->
            let
                newTodos =
                    Todos.deleteItem itemId model.todos
            in
            { model | todos = newTodos }

        EditItem item ->
            let
                edit =
                    { id = item.id
                    , text = item.text
                    }
            in
            { model | editingItem = Just edit }

        UpdateEditText updatedText ->
            let
                edit =
                    model.editingItem
                        |> Maybe.map (\item -> { item | text = updatedText })
            in
            { model | editingItem = edit }

        CancelEdit ->
            { model | editingItem = Nothing }

        FinishEdit ->
            let
                ( updatedItem, newTodos ) =
                    model.editingItem
                        |> Maybe.map
                            (\editingItem ->
                                Todos.setText editingItem.text editingItem.id model.todos
                            )
                        |> Maybe.withDefault ( Nothing, model.todos )
            in
            { model
                | todos = newTodos
                , editingItem = Nothing
            }

        ClearCompleted ->
            let
                delete todos id =
                    Todos.deleteItem id todos

                deletedTodos =
                    Todos.completedTodos model.todos
                        |> List.foldl
                            (\item todos ->
                                Todos.deleteItem item.id model.todos
                            )
                            model.todos
            in
            { model | todos = deletedTodos }

        ToggleAll setCompleted ->
            let
                toggle todos id =
                    Todos.setCompleted setCompleted id todos

                toggledTodos =
                    Todos.allTodos model.todos
                        |> List.foldl
                            (\item todos ->
                                toggle todos item.id
                                    |> Tuple.second
                            )
                            model.todos
            in
            { model | todos = toggledTodos }


view : Model -> Html Message
view model =
    H.section
        [ Attr.class "todoapp" ]
        [ viewHeader model
        , viewMain model
        , viewFooter model
        ]


viewHeader : Model -> Html Message
viewHeader model =
    H.header
        [ Attr.class "header" ]
        [ H.h1 [] [ H.text "todos" ]
        , H.input
            [ Attr.class "new-todo"
            , Attr.placeholder "what needs to be done?"
            , Attr.autofocus True
            , Attr.value model.newText
            , Ev.onInput UpdateNewText
            , onFinish AddTodo ClearNewText
            , Ev.onBlur ClearNewText
            ]
            []
        ]


viewMain : Model -> Html Message
viewMain model =
    if Todos.isEmpty model.todos then
        H.text ""

    else
        H.section
            [ Attr.class "main" ]
            [ H.input
                [ Attr.id "toggle-all"
                , Attr.class "toggle-all"
                , Attr.type_ "checkbox"
                , Attr.checked (Todos.allCompleted model.todos)
                , Ev.onCheck ToggleAll
                ]
                []
            , H.label
                [ Attr.for "toggle-all" ]
                [ H.text "Mark all as complete" ]
            , H.ul
                [ Attr.class "todo-list" ]
                (Todos.allTodos model.todos |> List.map (viewItem model.editingItem))
            ]


viewItem : Maybe { id : Todos.Id, text : String } -> Todos.Item -> Html Message
viewItem editing item =
    let
        isEditing =
            Maybe.map .id editing == Just item.id
    in
    H.li
        [ Attr.classList
            [ ( "completed", item.completed )
            , ( "editing", isEditing )
            ]
        ]
        [ H.div
            [ Attr.class "view" ]
            [ H.input
                [ Attr.class "toggle"
                , Attr.type_ "checkbox"
                , Attr.checked item.completed
                , Ev.onCheck (CheckItem item.id)
                ]
                []
            , H.label
                [ Ev.onDoubleClick (EditItem item)
                ]
                [ H.text item.text ]
            , H.button
                [ Attr.class "destroy"
                , Ev.onClick (DeleteItem item.id)
                ]
                []
            ]
        , H.input
            [ Attr.class "edit"
            , Attr.id ("edit_" ++ Todos.idToString item.id)
            , Attr.value (Maybe.map .text editing |> Maybe.withDefault "")
            , Ev.onInput UpdateEditText
            , onFinish FinishEdit CancelEdit
            , Ev.onBlur CancelEdit
            ]
            []
        ]


viewFooter : Model -> Html Message
viewFooter model =
    if Todos.isEmpty model.todos then
        H.text ""

    else
        H.footer
            [ Attr.class "footer" ]
            [ H.span [ Attr.class "todo-count" ]
                [ H.strong [] [ H.text (String.fromInt (itemCount model)) ]
                , H.text " item left"
                ]
            , H.button
                [ Attr.class "clear-completed"
                , Ev.onClick ClearCompleted
                ]
                [ H.text "Clear completed" ]
            ]


onFinish : Message -> Message -> H.Attribute Message
onFinish enterMessage escapeMessage =
    let
        select key =
            case key of
                13 ->
                    enterMessage

                27 ->
                    escapeMessage

                _ ->
                    NoOp
    in
    Ev.on "keydown" (Decode.map select Ev.keyCode)


itemCount : Model -> Int
itemCount model =
    List.length (Todos.allTodos model.todos)
