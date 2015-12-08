module Category (Model, init, initWithTuple, Action, update, view, Context, hasContent) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Graphics.Input.Field exposing(..)
import Json.Decode as Json
import String


-- MODEL

type alias ID = Int

type alias Model =
  { name : Content
  , color : Content
  , id : ID
  }

init : String -> String -> ID-> Model
init name color id =
  let
    emptySelection = Selection 0 0 Forward
  in
    { name = Content name emptySelection
    , color = Content color emptySelection
    , id = id
    }

initWithTuple : ( String, String, ID ) -> Model
initWithTuple ( name, color, id ) =
  let
    emptySelection = Selection 0 0 Forward
  in
    { name = Content name emptySelection
    , color = Content color emptySelection
    , id = id
    }


-- UPDATE

type Action
  = UpdateName Content
  | UpdateColor Content

type alias Context =
  { actions : Signal.Address Action
  , remove : Signal.Address ()
  }

update : Action -> Model -> Model
update action model =
  case action of
    UpdateName newName ->
      { model | name = newName }
    UpdateColor newColor ->
      { model | color = newColor }


-- VIEW

view : Context -> Model -> Html
view context model =
  let
    name = model.name.string
    nameField = field defaultStyle (nameUpdateMessage context.actions) "Name" model.name

    color = model.color.string
    colorField = field defaultStyle (colorUpdateMessage context.actions) "Color" model.color
  in
    li []
      [ fromElement nameField
      , fromElement colorField
      , text ("Name: " ++ name ++ ", Color: " ++ color)
      , button [onClick context.remove ()] [ text "X" ]
      ]

nameUpdateMessage : Signal.Address Action -> Content -> Signal.Message
nameUpdateMessage address content =
  Signal.message address (UpdateName content)

colorUpdateMessage : Signal.Address Action -> Content -> Signal.Message
colorUpdateMessage address content =
  Signal.message address (UpdateColor content)


countStyle : Attribute
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]

-- HELPERS
hasContent : String -> Model -> Bool
hasContent query category =
  String.contains query category.name.string
  || String.contains query category.color.string
