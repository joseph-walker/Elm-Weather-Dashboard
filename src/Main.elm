import StartApp
import Task exposing (Task)
import Html exposing (Html)
import Effects exposing (Never)
import CityList exposing (..)

port tasks : Signal (Task Never ())
port tasks =
    app.tasks

citiesToMonitor : List String
citiesToMonitor =
    [ "Tyler, Texas"
    , "San Francisco, California"
    , "London, England"
    , "Berlin, Germany"
    , "Moscow, Russia"
    , "Buenos Aires, Argentina"
    , "Stockholm, Sweden"
    ]

app : StartApp.App Model
app =
    StartApp.start
        { init = init citiesToMonitor
        , update = update
        , view = view
        , inputs = []
        }

main : Signal Html
main =
    app.html
