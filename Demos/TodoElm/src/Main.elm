module Main exposing (main)

import Api.Todos as Api
import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev exposing (onClick)
import Http
import Json.Decode as Json
import Route exposing (Route(..))
import Task
import Todos exposing (Todos)
import Url exposing (Url)


main : Program Flags Model Message
main =
    Browser.application
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Flags =
    { baseUrl : String }


type alias Model =
    { todos : Todos
    , newText : String
    , editingItem : Maybe { id : Todos.Id, text : String }
    , activeFilter : Filter
    , navKey : Nav.Key
    , flags : Flags
    }


type Filter
    = All
    | Active
    | Completed


type Message
    = NoOp
    | UrlRequested UrlRequest
    | UrlChanged Url
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
    | ItemsReceived (Result Http.Error (List Todos.Item))
    | ItemReceived (Result Http.Error Todos.Item)


initialModel : Flags -> Url -> Nav.Key -> ( Model, Cmd Message )
initialModel flags url navKey =
    ( { todos = Todos.empty
      , newText = ""
      , editingItem = Nothing
      , activeFilter = urlToFilter url
      , navKey = navKey
      , flags = flags
      }
    , Cmd.batch
        [ Route.fromUrl url |> Maybe.map (always Cmd.none) |> Maybe.withDefault (Nav.replaceUrl navKey (Route.toString RouteAll))
        , Api.getAll flags.baseUrl ItemsReceived
        ]
    )


itemCount : Model -> Int
itemCount model =
    List.length (filtered model.activeFilter model.todos)


filtered : Filter -> Todos -> List Todos.Item
filtered filter =
    case filter of
        All ->
            Todos.allTodos

        Active ->
            Todos.activeTodos

        Completed ->
            Todos.completedTodos


urlToFilter : Url -> Filter
urlToFilter url =
    case Route.fromUrl url of
        Just RouteAll ->
            All

        Just RouteActive ->
            Active

        Just RouteCompleted ->
            Completed

        Nothing ->
            All


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case Route.fromUrl url of
                        Nothing ->
                            ( model, Nav.replaceUrl model.navKey (Route.toString RouteAll) )

                        Just route ->
                            ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged newUrl ->
            ( { model | activeFilter = urlToFilter newUrl }, Cmd.none )

        UpdateNewText updatedText ->
            ( { model | newText = updatedText }, Cmd.none )

        ClearNewText ->
            ( { model | newText = "" }, Cmd.none )

        AddTodo ->
            let
                insertCmd =
                    Api.new model.flags.baseUrl ItemReceived model.newText
            in
            ( { model | newText = "" }, insertCmd )

        CheckItem itemId completed ->
            let
                ( newItem, newTodos ) =
                    Todos.setCompleted completed itemId model.todos

                updateCmd =
                    newItem
                        |> Maybe.map (Api.update model.flags.baseUrl ItemReceived)
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | todos = newTodos }, updateCmd )

        DeleteItem itemId ->
            let
                deleteCmd =
                    Api.delete model.flags.baseUrl ItemsReceived itemId

                newTodos =
                    Todos.deleteItem itemId model.todos
            in
            ( { model | todos = newTodos }, deleteCmd )

        EditItem item ->
            let
                edit =
                    { id = item.id
                    , text = item.text
                    }
            in
            ( { model | editingItem = Just edit }
            , Task.attempt (always NoOp) (Dom.focus ("edit_" ++ Todos.idToString item.id))
            )

        UpdateEditText updatedText ->
            let
                edit =
                    model.editingItem
                        |> Maybe.map (\item -> { item | text = updatedText })
            in
            ( { model | editingItem = edit }, Cmd.none )

        CancelEdit ->
            ( { model | editingItem = Nothing }, Cmd.none )

        FinishEdit ->
            let
                ( updatedItem, newTodos ) =
                    model.editingItem
                        |> Maybe.map
                            (\editingItem ->
                                Todos.setText editingItem.text editingItem.id model.todos
                            )
                        |> Maybe.withDefault ( Nothing, model.todos )

                updateCmd =
                    updatedItem
                        |> Maybe.map (Api.update model.flags.baseUrl ItemReceived)
                        |> Maybe.withDefault Cmd.none
            in
            ( { model
                | todos = newTodos
                , editingItem = Nothing
              }
            , updateCmd
            )

        ClearCompleted ->
            let
                delete todos id =
                    Todos.deleteItem id todos

                ( deleteCmds, deletedTodos ) =
                    Todos.completedTodos model.todos
                        |> List.foldl
                            (\item ( cmds, todos ) ->
                                let
                                    newTodos =
                                        Todos.deleteItem item.id model.todos

                                    deleteCmd =
                                        Api.delete model.flags.baseUrl ItemsReceived item.id
                                in
                                ( deleteCmd :: cmds, newTodos )
                            )
                            ( [], model.todos )
            in
            ( { model | todos = deletedTodos }, Cmd.batch deleteCmds )

        ToggleAll setCompleted ->
            let
                toggle todos id =
                    Todos.setCompleted setCompleted id todos

                ( toggleCmds, toggledTodos ) =
                    filtered model.activeFilter model.todos
                        |> List.foldl
                            (\item ( cmds, todos ) ->
                                let
                                    ( toggledItem, newTodos ) =
                                        toggle todos item.id

                                    toggleCmd =
                                        toggledItem
                                            |> Maybe.map (Api.update model.flags.baseUrl ItemReceived)
                                            |> Maybe.withDefault Cmd.none
                                in
                                ( toggleCmd :: cmds, newTodos )
                            )
                            ( [], model.todos )
            in
            ( { model | todos = toggledTodos }, Cmd.batch toggleCmds )

        ItemsReceived (Ok items) ->
            ( { model | todos = Todos.fromList items }, Cmd.none )

        ItemsReceived (Err _) ->
            -- for this Demo we ignore communication errors - sorry
            ( model, Cmd.none )

        ItemReceived (Ok item) ->
            ( { model | todos = Todos.insertItem item model.todos }, Cmd.none )

        ItemReceived (Err _) ->
            -- for this Demo we ignore communication errors - sorry
            ( model, Cmd.none )


view : Model -> Document Message
view model =
    { title = "TODO - Elm"
    , body = [ viewTodoApp model ]
    }


viewTodoApp : Model -> Html Message
viewTodoApp model =
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


{-| should be hidden by default and shown when there are todos
-}
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

            -- li-items with class completed (if it is), containing viewItem or editItem
            , H.ul
                [ Attr.class "todo-list" ]
                (filtered model.activeFilter model.todos |> List.map (viewItem model.editingItem))
            ]


{-| should display an item
-}
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


{-| should be hidden by default and shown when there are todos
-}
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
            , H.ul [ Attr.class "filters" ]
                [ H.li []
                    [ H.a
                        [ Attr.classList [ ( "selected", model.activeFilter == All ) ]
                        , Attr.href (Route.toString RouteAll)
                        ]
                        [ H.text "All" ]
                    ]
                , H.li []
                    [ H.a
                        [ Attr.classList [ ( "selected", model.activeFilter == Active ) ]
                        , Attr.href (Route.toString RouteActive)
                        ]
                        [ H.text "Active" ]
                    ]
                , H.li []
                    [ H.a
                        [ Attr.classList [ ( "selected", model.activeFilter == Completed ) ]
                        , Attr.href (Route.toString RouteCompleted)
                        ]
                        [ H.text "Completed" ]
                    ]
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
                    -- Not a 'finish' key, such as ENTER or ESCAPE
                    NoOp
    in
    Ev.on "keydown" (Json.map select Ev.keyCode)
