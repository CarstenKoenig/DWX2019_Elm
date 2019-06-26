module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev


main : Program () Model Message
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


type alias Model =
    {}


type Message
    = NoOp


initialModel : Model
initialModel =
    {}


update : Message -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Message
view model =
    H.text "have some Elm"
