module ContactBook where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, (:=))
import Graphics.Input.Field exposing(..)
import Effects exposing (Effects, Never)
import Http
import Task
import String
import Category


-- MODEL
importUrl = "https://contactsampleprovider.herokuapp.com"

type alias Model =
  { categories : List Category.Model
  , nextID : ID
  , filterQuery : Content
  }

type alias ID = Int

init : ( Model, Effects Action )
init =
  ({ categories = []
  , nextID = 0
  , filterQuery = Content "" (Selection 0 0 Forward)
  }, Effects.none )

-- UPDATE

type Action
  = Insert
  | StartImport
  | ProcessImport (Maybe (List (String, String, Int)))
  | Filter Content
  | Remove ID
  | Modify ID Category.Action

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Insert ->
      let
        newCategory = ( Category.init "new" "category" model.nextID )
        newCategories = model.categories ++ [ newCategory ]
      in
        ({ model |
          categories = newCategories,
          nextID = model.nextID + 1
        }, Effects.none)
    StartImport ->
      (model, getContacts)
    ProcessImport stuff ->
      let
        newModel = case stuff of
          Just anything -> processImport anything
          Nothing -> ( model, Effects.none )
      in
        newModel
    Filter content ->
      ({ model | filterQuery = content }, Effects.none)
    Remove id ->
      ({ model |
          categories = List.filter (\(cat) -> id /= cat.id) model.categories
      }, Effects.none)
    Modify id categoryAction->
      let updateCat categoryModel =
        if categoryModel.id == id then
          Category.update categoryAction categoryModel
        else
          categoryModel
      in
          ({ model | categories = List.map updateCat model.categories }, Effects.none)


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    filteredCategories = List.filter (Category.hasContent model.filterQuery.string) model.categories
    categories = List.map (viewCategory address) filteredCategories
    insert = button [ onClick address Insert ] [ text "Add" ]
    importButton = button [ onClick address StartImport ] [ text "Import" ]
    filterField = field defaultStyle (queryUpdateMessage address) "Search" model.filterQuery
  in
    div []
      [ div [] [ importButton ]
      , div [] [ fromElement filterField ]
      , div [] [ insert ]
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

getContacts : Effects Action
getContacts =
  Http.get categoriesDecoder importUrl
    |> Task.toMaybe
    |> Task.map ProcessImport
    |> Effects.task

-- DECODERS
categoriesDecoder : Decoder (List (String, String, Int))
categoriesDecoder = Decode.at ["categories"] (Decode.list categoryDecoder)

categoryDecoder : Decoder (String, String, Int)
categoryDecoder =
  Decode.object3 (,,)
    ("name" := Decode.string)
    ("color" := Decode.string)
    ("id" := Decode.int)

processImport : (List (String, String, Int)) -> (Model, Effects Action)
processImport stuff =
  let
    (newModel, newAction) = init
    newCategories = List.map Category.initWithTuple stuff
  in
    ({ newModel | categories = newCategories }, newAction)
