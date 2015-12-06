module ContactBook where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import String
import Category


-- MODEL

type alias Model =
  { categories : List ( ID, Category.Model)
  , nextID : ID
  }

type alias ID = Int

init : Model
init =
    { categories = []
    , nextID = 0
    }


-- UPDATE

type Action
      = Insert
      | Remove ID
      | Modify ID Category.Action

update : Action -> Model -> Model
update action model =
  case action of
    Insert ->
      let newCategory = ( model.nextID, Category.init "new" "category" )
          newCategories = model.categories ++ [ newCategory ]
      in
          { model |
              categories = newCategories,
              nextID = model.nextID + 1
          }
    Remove id ->
      { model |
          categories = List.filter (\(counterID, _) -> counterID /= id) model.categories
      }
    Modify id categoryAction->
      let updateCat (catID, categoryModel) =
        if catID == id then
          (catID, Category.update categoryAction categoryModel)
        else
          (catID, categoryModel)
      in
          { model | categories = List.map updateCat model.categories }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    categories = List.map (viewCategory address) model.categories
    insert = button [ onClick address Insert ] [ text "Add" ]
  in
    div [] [  insert
           , ul [] categories
           ]


viewCategory : Signal.Address Action -> (ID, Category.Model) -> Html
viewCategory address (id, model) =
  let
    context = Category.Context
      ( Signal.forwardTo address ( Modify id))
      ( Signal.forwardTo address ( always (Remove id)))
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
