module Category (Model, init, Action, update, view, Context, withContent) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Graphics.Input.Field exposing(..)
import Json.Decode as Json
import String


-- MODEL

type alias Model =
  { name : Content
  , color : Content
  }

init: String -> String -> Model
init name color =
  let
    emptySelection = Selection 0 0 Forward
  in
    { name = Content name emptySelection
    , color = Content color emptySelection
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
    nameContent = Content name model.name.selection
    nameField = field defaultStyle (nameUpdateMessage context.actions) "Name" nameContent

    color = model.color.string
    colorContent = Content color model.color.selection
    colorField = field defaultStyle (colorUpdateMessage context.actions) "Color" colorContent
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
withContent : String -> Model -> Bool
withContent query category =
  String.contains query category.name.string
  || String.contains query category.color.string
