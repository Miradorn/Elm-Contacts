module ContactBook where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Graphics.Input.Field exposing(..)
import String
import Category


-- MODEL

type alias Model =
  { categories : List Category.Model
  , nextID : ID
  , filterQuery : Content
  }

type alias ID = Int

init : Model
init =
  { categories = []
  , nextID = 0
  , filterQuery = Content "" (Selection 0 0 Forward)
  }

-- UPDATE

type Action
  = Insert
  | Filter Content
  | Remove ID
  | Modify ID Category.Action

update : Action -> Model -> Model
update action model =
  case action of
    Insert ->
      let
        newCategory = ( Category.init "new" "category" model.nextID )
        newCategories = model.categories ++ [ newCategory ]
      in
        { model |
          categories = newCategories,
          nextID = model.nextID + 1
        }
    Filter content ->
      { model | filterQuery = content }
    Remove id ->
      { model |
          categories = List.filter (\(cat) -> id /= cat.id) model.categories
      }
    Modify id categoryAction->
      let updateCat categoryModel =
        if categoryModel.id == id then
          Category.update categoryAction categoryModel
        else
          categoryModel
      in
          { model | categories = List.map updateCat model.categories }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    filteredCategories = List.filter (Category.hasContent model.filterQuery.string) model.categories
    categories = List.map (viewCategory address) filteredCategories
    insert = button [ onClick address Insert ] [ text "Add" ]
    filterField = field defaultStyle (queryUpdateMessage address) "Search" model.filterQuery
  in
    div []
      [ insert
      , fromElement filterField
      , ul [] categories
      ]

queryUpdateMessage : Signal.Address Action -> Content -> Signal.Message
queryUpdateMessage address content =
  Signal.message address (Filter content)

viewCategory : Signal.Address Action -> Category.Model -> Html
viewCategory address model =
  let
    context = Category.Context
      ( Signal.forwardTo address ( Modify model.id))
      ( Signal.forwardTo address ( always (Remove model.id)))
  in
    Category.view context model

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
