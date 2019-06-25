module Route exposing (Route(..), fromUrl, toString)

import Url exposing (Url)
import Url.Parser as Route exposing (Parser)


type Route
    = RouteAll
    | RouteActive
    | RouteCompleted


fromUrl : Url -> Maybe Route
fromUrl =
    Route.parse routeP


toString : Route -> String
toString route =
    case route of
        RouteAll ->
            "/"

        RouteActive ->
            "/active"

        RouteCompleted ->
            "/completed"


routeP : Parser (Route -> a) a
routeP =
    Route.oneOf
        [ Route.map RouteAll Route.top
        , Route.map RouteActive (Route.s "active")
        , Route.map RouteCompleted (Route.s "completed")
        ]
