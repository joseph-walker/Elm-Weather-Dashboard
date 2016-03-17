module CityList where

import Html exposing (..)
import Effects exposing (Effects)
import Array exposing (Array, map, set, get, indexedMap, fromList, toList)
import List
import City

-- Types

type alias Model =
    { cities : Array City.Model
    }

type Action
    = ListAction Int City.Action

-- Model / View / Update

init : List String -> (Model, Effects Action)
init cities =
    let
        initialModels : Array (City.Model, Effects City.Action)
        initialModels =
            fromList
            <| List.map City.init cities
    in
        ( Model
            <| map fst initialModels
        , Effects.batch
            <| toList
            <| indexedMap (\index (model, effects) -> Effects.map (ListAction index) effects) initialModels
        )

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        ListAction index cityAction ->
            case get index model.cities of
                Just city ->
                    let
                        (newCityModel, newCityEffects) =
                            City.update cityAction city
                    in
                        ( { model | cities = set index newCityModel model.cities }
                        , Effects.map (ListAction index) newCityEffects
                        )
                Nothing ->
                    ( model
                    , Effects.none
                    )

view : Signal.Address Action -> Model -> Html
view address model =
    ul []
        <| toList
        <| indexedMap (viewCity address) model.cities

viewCity : Signal.Address Action -> Int -> City.Model -> Html
viewCity address id model =
    li [] [ City.view (Signal.forwardTo address (ListAction id)) model ]
