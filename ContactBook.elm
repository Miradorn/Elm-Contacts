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


-- MODEL
importUrl = "https://contactsampleprovider.herokuapp.com"

type alias Category =
  { name : Content
  , color : Content
  , id : ID
  }

type alias Model =
  { categories : List Category
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


initCategory : ( String, String, ID ) -> Category
initCategory ( name, color, id ) =
  let
    emptySelection = Selection 0 0 Forward
  in
    { name = Content name emptySelection
    , color = Content color emptySelection
    , id = id
    }
-- UPDATE

type Action
  = Insert
  | StartImport
  | ProcessImport (Maybe (List (String, String, Int)))
  | Filter Content
  | Remove ID
  | ModifyCategoryName ID Content
  | ModifyCategoryColor ID Content

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Insert ->
      let
        newCategory =  initCategory ((,,) "new" "category" model.nextID)
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
    ModifyCategoryName id name->
      let updateCat categoryModel =
        if categoryModel.id == id then
          {categoryModel | name = name}
        else
          categoryModel
      in
          ({ model | categories = List.map updateCat model.categories }, Effects.none)
    ModifyCategoryColor id color ->
      let updateCat categoryModel =
        if categoryModel.id == id then
          {categoryModel | color = color}
        else
          categoryModel
      in
          ({ model | categories = List.map updateCat model.categories }, Effects.none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    filteredCategories = List.filter (categoryHasContent model.filterQuery.string) model.categories
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

viewCategory : Signal.Address Action -> Category -> Html
viewCategory address category =
  let
    name = category.name.string
    nameField = field defaultStyle (Signal.message (Signal.forwardTo address (ModifyCategoryName category.id))) "Name" category.name

    color = category.color.string
    colorField = field defaultStyle (Signal.message (Signal.forwardTo address (ModifyCategoryColor category.id))) "Color" category.color
  in
    li [ listStyle category.color.string ]
      [ fromElement nameField
      , fromElement colorField
      , text ("Name: " ++ name ++ ", Color: " ++ color)
      , button [onClick address (Remove category.id)] [ text "X" ]
      ]


queryUpdateMessage : Signal.Address Action -> Content -> Signal.Message
queryUpdateMessage address content =
  Signal.message address (Filter content)


listStyle : String -> Attribute
listStyle color =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("color", color)
    ]

-- HELPERS

getContacts : Effects Action
getContacts =
  Http.get categoriesDecoder importUrl
    |> Task.toMaybe
    |> Task.map ProcessImport
    |> Effects.task


categoryHasContent : String -> Category -> Bool
categoryHasContent query category =
  String.contains query category.name.string
  || String.contains query category.color.string

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
    newCategories = List.map initCategory stuff
  in
    ({ newModel | categories = newCategories }, newAction)
