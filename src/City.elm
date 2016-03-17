module City where

import Http
import Task
import String
import Html exposing (..)
import Effects exposing (Effects)
import Json.Decode as Json exposing ((:=), Decoder)

-- Types

type alias Model =
    { city : String
    , temp : Temperature
    }

type Temperature
    = Degrees Int
    | Loading

type Action
    = UpdateTemp (Maybe Temperature)

-- Model / View / Update

init : String -> (Model, Effects Action)
init city =
    ( Model city Loading
    , getTemperature city
    )

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        UpdateTemp temp ->
            case temp of
                Just deg ->
                    (Model model.city deg, Effects.none)
                Nothing ->
                    (Model model.city model.temp, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model =
    case model.temp of
        Degrees temperature ->
            text <| model.city ++ ": " ++ (toString temperature) ++ "°F"
        Loading ->
            text <| model.city ++ ": Loading..."

-- Effects

getTemperature : String -> Effects Action
getTemperature city =
    Http.get weatherDecoder (apiCityEndpoint city)
        |> Task.toMaybe
        |> Task.map UpdateTemp
        |> Effects.task

weatherDecoder : Decoder Temperature
weatherDecoder =
    Json.at ["data", "current_condition"]
        <| Json.tuple1 (Degrees << Result.withDefault 0 << String.toInt)
        <| "temp_F" := Json.string

apiCityEndpoint : String -> String
apiCityEndpoint city =
    "https://api.worldweatheronline.com/free/v2/weather.ashx?key=43a48c6ed87a491aae4183801161503&q=" ++ (Http.uriEncode city) ++ "&date=today&fx=no&format=json&showlocaltime=yes"
