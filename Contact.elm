module Contact (Model, init, Action, update, view) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Graphics.Input.Field exposing(..)
import Json.Decode as Json
import String


-- MODEL

type alias Model =
  { name : Content
  , company : Content
  , addresses : List Content
  , phoneNumbers : List Content
  , mailAdresses : List Content
  }


init: String -> String -> List String -> List String -> List String -> Model
init name company addresses phoneNumbers mailAdresses =
  let
    emptySelection = Selection 0 0 Forward
  in
    { name = Content name emptySelection
    , company = Content company emptySelection
    , addresses = List.map ( \address -> Content address emptySelection)  addresses
    , phoneNumbers = List.map ( \number -> Content number emptySelection) phoneNumbers
    , mailAdresses = List.map ( \address -> Content address emptySelection) mailAdresses
    }
-- UPDATE

type Action
  = UpdateName Content
  | UpdateCompany Content
  | Remove



update : Action -> Model -> Model
update action model =
  case action of
    UpdateName newName ->
      { model | name = newName }
    UpdateCompany newCompany ->
      { model | company = newCompany }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    name = model.name.string
    nameContent = Content name model.name.selection
    nameField = field defaultStyle (nameUpdateMessage Signal.Address Action) "Name" name

    color = model.color.string
    companyContent = Content color model.color.selection
    colorField = field defaultStyle (colorUpdateMessage Signal.Address Action) "company" color
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

companyUpdateMessage : Signal.Address Action -> Content -> Signal.Message
companyUpdateMessage address content =
  Signal.message address (UpdateCompany content)


countStyle : Attribute
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]
