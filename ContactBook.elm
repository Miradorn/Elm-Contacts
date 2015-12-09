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

type alias Contact =
  { name : Content
  , company : Content
  , addresses : List Content
  , phones : List Content
  , emails : List Content
  , category : ID
  , id : ID
  }

type alias Category =
  { name : Content
  , color : Content
  , id : ID
  }

type alias Model =
  { categories : List Category
  , nextCategoryID : ID
  , contacts : List Contact
  , nextContactID : ID
  , filterQuery : Content
  }

type alias ID = Int

init : ( Model, Effects Action )
init =
  (
    { categories = []
    , contacts = []
    , nextCategoryID = 0
    , nextContactID = 0
    , filterQuery = Content "" (Selection 0 0 Forward)
    }
  , Effects.none )


initCategory : CategoryImportType -> Category
initCategory ( name, color, id ) =
  let
    emptySelection = Selection 0 0 Forward
  in
    { name = Content name emptySelection
    , color = Content color emptySelection
    , id = id
    }

initContact :  ID -> ContactImportType -> Contact
initContact id ( name, company, addresses, phones, mails, category) =
  let
    emptySelection = Selection 0 0 Forward
    newAddresses = List.map (\address -> Content address emptySelection) addresses
    newPhones = List.map (\phones -> Content phones emptySelection) phones
    newMails = List.map (\mail -> Content mail emptySelection) mails
  in
    { name = Content name emptySelection
    , company = Content company emptySelection
    , addresses = newAddresses
    , phones = newPhones
    , emails = newMails
    , category = category
    , id = id
    }

-- UPDATE

type Action
  = Insert
  | StartImport
  | ProcessImport (Maybe (List CategoryImportType, List ContactImportType))
  | Filter Content
  | Remove ID
  | ModifyCategoryName ID Content
  | ModifyCategoryColor ID Content
  | ModifyContactName ID Content
  | ModifyContactCompany ID Content

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Insert ->
      let
        newCategory =  initCategory ((,,) "new" "category" model.nextCategoryID)
        newCategories = model.categories ++ [ newCategory ]
      in
        ({ model |
          categories = newCategories,
          nextCategoryID = model.nextCategoryID + 1
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
    ModifyCategoryName id name ->
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
    ModifyContactName id name ->
      let updateContact contactModel =
        if contactModel.id == id then
          {contactModel | name = name}
        else
          contactModel
      in
        ({ model | contacts = List.map updateContact model.contacts }, Effects.none)
    ModifyContactCompany id company ->
      let updateContact contactModel =
        if contactModel.id == id then
          {contactModel | company = company}
        else
          contactModel
      in
        ({ model | contacts = List.map updateContact model.contacts }, Effects.none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    filteredCategories = List.filter (categoryHasContent model.filterQuery.string) model.categories
    categories = List.map (\cat -> viewCategory address cat (contactsWithCategory model.contacts cat.id)) filteredCategories
    insert = button [ onClick address Insert ] [ text "Add" ]
    importButton = button [ onClick address StartImport ] [ text "Import" ]
    filterField = field defaultStyle (queryUpdateMessage address) "Search" model.filterQuery
  in
    div [style [("background-color", "black")]]
      [ div [] [ importButton ]
      , div [] [ fromElement filterField ]
      , div [] [ insert ]
      , ul [] categories
      ]

viewCategory : Signal.Address Action -> Category -> List Contact -> Html
viewCategory address category contacts =
  let
    name = category.name.string
    nameField = field defaultStyle (Signal.message (Signal.forwardTo address (ModifyCategoryName category.id))) "Name" category.name

    color = category.color.string
    colorField = field defaultStyle (Signal.message (Signal.forwardTo address (ModifyCategoryColor category.id))) "Color" category.color
  in
    li [ listStyle category.color.string ]
      [ div []
        [ div [] [text ("Name: " ++ name ++ ", Color: " ++ color)]
        , fromElement nameField
        , fromElement colorField
        , button [onClick address (Remove category.id)] [ text "X" ]
        ]
      , div []
        [ ul [] (List.map (viewContact address) contacts)

        ]
     ]



viewContact : Signal.Address Action -> Contact -> Html
viewContact address contact =
  let
    name = contact.name.string
    nameField = field defaultStyle (Signal.message (Signal.forwardTo address (ModifyContactName contact.id))) "Name" contact.name

    company = contact.company.string
    companyField = field defaultStyle (Signal.message (Signal.forwardTo address (ModifyContactCompany contact.id))) "Name" contact.company
  in
    li []
      [ div [] [text ("name:" ++ contact.name.string ++ "company: " ++ contact.company.string) ]
      , fromElement nameField
      , fromElement companyField
      ]

viewAddress : Signal.Address Action -> Content -> Html
viewAddress address addressContent =
  text addressContent.string

queryUpdateMessage : Signal.Address Action -> Content -> Signal.Message
queryUpdateMessage address content =
  Signal.message address (Filter content)

-- STYLES

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
  Http.get contactBookDecoder importUrl
    |> Task.toMaybe
    |> Task.map ProcessImport
    |> Effects.task

type alias ContactImportType =
  (String, String, List String, List String, List String, ID)

type alias CategoryImportType =
  (String, String, ID)

processImport : (List CategoryImportType, List ContactImportType) -> (Model, Effects Action)
processImport (importedCategories, importedContacts) =
  let
    (newModel, newAction) = init

    newCategories = List.map initCategory importedCategories
    ids = List.map (\(_, _, id) -> id) importedCategories
    catMaxId = Maybe.withDefault 0 (List.maximum ids)

    indexRange = [0..(List.length importedContacts)]

    newContacts = List.map2 initContact indexRange importedContacts
    newNextContactID = List.length indexRange
  in
    (
    { newModel
      | categories = newCategories
      , nextCategoryID = catMaxId + 1
      , contacts = newContacts
      , nextContactID = newNextContactID
    }
      , newAction
      )

contactsWithCategory : List Contact -> ID -> List Contact
contactsWithCategory contacts categoryID =
  List.filter (\contact -> contact.category == categoryID) contacts

categoryHasContent : String -> Category -> Bool
categoryHasContent query category =
  String.contains (String.toLower query) (String.toLower category.name.string)
  || String.contains query category.color.string

-- DECODERS

contactBookDecoder : Decoder (List CategoryImportType, List ContactImportType)
contactBookDecoder =
  Decode.object2 (,)
    (Decode.at ["categories"] categoriesDecoder)
    (Decode.at ["contacts"] contactsDecoder)

categoriesDecoder : Decoder (List CategoryImportType)
categoriesDecoder = (Decode.list categoryDecoder)

categoryDecoder : Decoder CategoryImportType
categoryDecoder =
  Decode.object3 (,,)
    ("name" := Decode.string)
    ("color" := Decode.string)
    ("id" := Decode.int)

contactsDecoder : Decoder (List ContactImportType)
contactsDecoder = (Decode.list contactDecoder)

contactDecoder : Decoder ContactImportType
contactDecoder =
  Decode.object6 (,,,,,)
    ("name" := Decode.string)
    ("company" := Decode.string)
    ("addresses" := Decode.list Decode.string)
    ("phones" := Decode.list Decode.string)
    ("emails" := Decode.list Decode.string)
    ("category" := Decode.int)
