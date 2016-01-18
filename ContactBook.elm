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
import Dict
import Set
import Text


-- MODEL
importUrl = "https://contactsampleprovider.herokuapp.com"
domainsToFilter =
  [ "web.de"
  , "gmail.com"
  , "t-online.de"
  , "gmx.net"
  , "gmx.de"
  , "googlemail.com"
  , "arcor.de"
  , "alice-dsl.de"
  ]

type alias Model =
  { categories : List Category
  , nextCategoryID : ID
  , contacts : List Contact
  , nextContactID : ID
  , filterQuery : Content
  , viewMode : ViewMode
  }

type alias Category =
  { name : Content
  , color : Content
  , id : ID
  }

type alias Contact =
  { name : Content
  , company : Content
  , addresses : List ContactContent
  , phones : List ContactContent
  , emails : List ContactContent
  , birthday : Content
  , category : ID
  , categoryObject : Maybe Category
  , id : ID
  }

type alias ContactContent =
  { text : Content
  , id : ID
  , contact : ID
  }

type ViewMode
  = Index
  | ViewCategory Category
  | ViewEmailList Category
  | ViewAllContacts
  | ViewAllCompanies
  | ViewTLDs
  | ViewCompanyTLDs


type alias ID = Int

emptySelection = Selection 0 0 Forward

init : ( Model, Effects Action )
init =
  (
    { categories = []
    , contacts = []
    , nextCategoryID = 0
    , nextContactID = 0
    , filterQuery = Content "" (Selection 0 0 Forward)
    , viewMode = Index
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
initContact id ( name, company, addresses, phones, mails, birthday, category) =
  let
    contentInitializer = initContactContent id

    newAddresses = List.indexedMap contentInitializer addresses
    newPhones = List.indexedMap contentInitializer phones
    newMails = List.indexedMap contentInitializer mails
  in
    { name = Content name emptySelection
    , company = Content company emptySelection
    , addresses = newAddresses
    , phones = newPhones
    , emails = newMails
    , birthday = Content birthday emptySelection
    , category = category
    , categoryObject = Nothing
    , id = id
    }

initContactContent :  ID -> ID -> String -> ContactContent
initContactContent contact id text =
  {id = id, contact = contact, text = Content text emptySelection}
-- UPDATE

type Action
  = Insert
  | StartImport
  | ProcessImport (Maybe (List CategoryImportType, List ContactImportType))
  | ShowIndex
  | ShowCategory Category
  | ShowEmailList Category
  | ShowAllContacts
  | ShowAllCompanies
  | ShowTLDs
  | ShowCompanyTLDs
  | Filter Content
  | RemoveCategory ID
  | ModifyCategoryName ID Content
  | ModifyCategoryColor ID Content
  | AddContact Category
  | RemoveContact ID
  | ModifyContactName ID Content
  | ModifyContactBirthday ID Content
  | ModifyContactCompany ID Content
  | ModifyContactContent ContactContent Content

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
    RemoveCategory id ->
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
    ShowIndex ->
      ({model | viewMode = Index}, Effects.none)
    ShowCategory category ->
      ({model | viewMode = ViewCategory category}, Effects.none)
    ShowEmailList category ->
      ({model | viewMode = ViewEmailList category}, Effects.none)
    ShowAllContacts ->
      ({model | viewMode = ViewAllContacts}, Effects.none)
    ShowAllCompanies ->
      ({model | viewMode = ViewAllCompanies}, Effects.none)
    ShowTLDs ->
      ({model | viewMode = ViewTLDs}, Effects.none)
    ShowCompanyTLDs ->
      ({model | viewMode = ViewCompanyTLDs}, Effects.none)
    AddContact category ->
      let
        newContact = initContact model.nextContactID ("", "", [], [], [], "", category.id)
        newContacts =  [ newContact ] ++ model.contacts
      in
        ({ model
          | contacts = newContacts
          , nextContactID = model.nextContactID + 1
        }, Effects.none)
    RemoveContact id ->
      ({ model |
          contacts = List.filter (\(con) -> id /= con.id) model.contacts
      }, Effects.none)
    ModifyContactName id name ->
      let updateContact contactModel =
        if contactModel.id == id then
          {contactModel | name = name}
        else
          contactModel
      in
        ({ model | contacts = List.map updateContact model.contacts }, Effects.none)
    ModifyContactBirthday id date ->
      let updateContact contactModel =
        if contactModel.id == id then
          {contactModel | birthday = date}
        else
          contactModel
      in
        ({ model | contacts = List.map updateContact model.contacts}, Effects.none)
    ModifyContactCompany id company ->
      let updateContact contactModel =
        if contactModel.id == id then
          {contactModel | company = company}
        else
          contactModel
      in
        ({ model | contacts = List.map updateContact model.contacts }, Effects.none)
    ModifyContactContent contactContent realContent ->
      let
        updateFuncs = [
          (\con -> con.addresses, \con val -> {con | addresses = val}),
          (\con -> con.phones, \con val -> {con | phones = val}),
          (\con -> con.emails, \con val -> {con | emails = val})
        ]

        updateContactModel (accessCon, updateCon) contactModel =
          let
            contentList = accessCon contactModel
            updatedContentList =
              if contactModel.id == contactContent.contact then
                List.map (updateFunc) contentList
              else
                contentList
          in
            updateCon contactModel updatedContentList

        updateContact contactModel =
          List.foldl updateContactModel contactModel updateFuncs

        updateFunc content =
          if content == contactContent then
            { contactContent | text = realContent }
          else
            content
      in
        ({ model | contacts = List.map updateContact model.contacts}, Effects.none)

-- VIEW

blankStyle =
  { padding = uniformly 0
  , outline = noOutline
  , highlight = noHighlight
  , style = Text.defaultStyle
  }

view : Signal.Address Action -> Model -> Html
view address model =
  let viewFinder =
    address
      |> case model.viewMode of
          Index -> viewIndex
          ViewCategory category -> viewCategory category
          ViewEmailList category -> viewEmailList category
          ViewAllContacts -> viewAllContacts
          ViewAllCompanies -> viewCompanies
          ViewTLDs -> viewTLDs
          ViewCompanyTLDs -> viewCompanyTLDs
  in
    viewFinder model

viewIndex : Signal.Address Action -> Model -> Html
viewIndex address model =
  let
    filteredCategories = List.filter (categoryHasContent model.filterQuery.string) model.categories |> List.reverse
    categories = List.map (\cat -> viewForCategory address cat) filteredCategories
    insert = button [ onClick address Insert ] [ text "Add" ]
    importButton = button [ onClick address StartImport ] [ text "Import" ]
    showAllButton = button [ onClick address ShowAllContacts ] [ text "Show All Contacts" ]
    showCompaniesButton = button [ onClick address ShowAllCompanies ] [ text "Show All Companies" ]
    showTLDsButton = button [ onClick address ShowTLDs ] [ text "Show All TLDs" ]
    showCompanyTLDsButton = button [ onClick address ShowCompanyTLDs ] [ text "Show Company TLDs" ]
  in
    div [class "container"]
      [ h3 [] [text "All categories" ]
      , div [class "filter_field"] [ filterField address model]
      , div [class "actions"]
        [ importButton
        , insert
        , showAllButton
        , showCompaniesButton
        , showTLDsButton
        , showCompanyTLDsButton
        ]
      , hr [] []
      , ul [] categories
      ]

viewForCategory : Signal.Address Action -> Category -> Html
viewForCategory address category =
  let
    name = category.name.string
    nameField = field defaultStyle (Signal.message (Signal.forwardTo address (ModifyCategoryName category.id))) "Name" category.name

    color = category.color.string
    colorField = field defaultStyle (Signal.message (Signal.forwardTo address (ModifyCategoryColor category.id))) "Color" category.color

    id = toString category.id
  in
    li [class "category"]
      [ h4 [] [ span [style [("background-color", category.color.string)], class "color"] []
        , text name
        ]
      , fromElement nameField
      , fromElement colorField
      , button [onClick address (RemoveCategory category.id)] [ text "X" ]
      , button [onClick address (ShowCategory category)] [text "Show"]
      ]

viewCategory : Category -> Signal.Address Action -> Model -> Html
viewCategory category address  model =
  let
    mappedContacts = contactsWithCategory model.contacts category.id
    filteredContacts = List.filter (contactHasContent model.filterQuery.string) mappedContacts
    contactsHtml = List.map (viewForContact address) filteredContacts

    addButton = button [onClick address (AddContact category)] [ text "Add" ]
    mailButton = button [onClick address (ShowEmailList category)] [ text "Send eMail" ]

    id = toString category.id
  in
    div [class "container"]
      [ h3 []
        [ text ("Category '" ++ category.name.string ++ "'")
        , span [class "color", style [("background-color", category.color.string)]] []
        ]
      , div [class "filter_field"] [ filterField address model]
      , div [class "actions"] [indexButton address, addButton, mailButton]
      , hr [] []
      , ul [] contactsHtml
      ]

viewEmailList : Category -> Signal.Address Action -> Model -> Html
viewEmailList category address  model =
  let
    notEmpty string = not (String.isEmpty string)
    mailGrabber contact =
      List.head contact.emails
        |> Maybe.map (\n -> n.text.string)
        |> Maybe.withDefault ""

    mappedContacts = contactsWithCategory model.contacts category.id
    emailList =
      List.map mailGrabber mappedContacts
      |> List.filter notEmpty
      |> String.join ", "

    id = toString category.id
  in
    div [class "container"]
      [ h3 []
        [ text ("All e-mail addresses in '" ++ category.name.string ++ "'")
        , span [class "color", style [("background-color", category.color.string)]] []
        ]
      , div [class "actions"]
        [ button [onClick address (ShowCategory category)] [text "Back"]
        , indexButton address
        ]
      , hr [] []
      , textarea [class "full_size"] [text emailList]
      ]

viewCompanies : Signal.Address Action -> Model -> Html
viewCompanies address model =
  let
    contactMapper contact =
      contact.company.string
    companies = List.map contactMapper model.contacts
      |> Set.fromList
      |> Set.toList
    filteredCompanies = List.filter (stringHasContent model.filterQuery.string) companies
    companiesHtml = List.map (\company -> li [] [text company]) filteredCompanies
  in
    div [class "container"]
      [ h3 [] [ text ("All companies") ]
      , div [class "filter_field"] [ filterField address model]
      , div [class "actions"] [indexButton address]
      , hr [] []
      , ul [class "plain"] companiesHtml
    ]

viewTLDs : Signal.Address Action -> Model -> Html
viewTLDs address model =
  let
    tldMapper email =
      email.text.string
        |> String.split "@"
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault ""
    contactToTLDMapper contact =
      List.map tldMapper contact.emails

    tldsHtml = List.map contactToTLDMapper model.contacts
      |> List.concat
      |> Set.fromList
      |> Set.remove ""
      |> Set.toList
      |> List.filter (stringHasContent model.filterQuery.string)
      |> List.map (\tld -> li [] [text tld])
  in
    div [class "container"]
      [ h3 [] [ text ("All TLDs") ]
      , div [class "filter_field"] [ filterField address model]
      , div [class "actions"] [indexButton address]
      , hr [] []
      , ul [class "plain"] tldsHtml
    ]

viewCompanyTLDs : Signal.Address Action -> Model -> Html
viewCompanyTLDs address model =
  let
    tldMapper email =
      email.text.string
        |> String.split "@"
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault ""
    contactToTLDMapper contact =
      List.map tldMapper contact.emails
    tldFilter mail =
      List.map (stringHasContent mail) domainsToFilter
        |> List.foldr (\a -> \b -> a || b) False
        |> not

    tldsHtml = List.map contactToTLDMapper model.contacts
      |> List.concat
      |> Set.fromList
      |> Set.remove ""
      |> Set.toList
      |> List.filter (stringHasContent model.filterQuery.string)
      |> List.filter tldFilter
      |> List.map (\tld -> li [] [text tld])
  in
    div [class "container"]
      [ h3 [] [ text ("Company TLDs") ]
      , div [class "filter_field"] [ filterField address model]
      , div [class "actions"] [indexButton address]
      , hr [] []
      , ul [class "plain"] tldsHtml
    ]

viewAllContacts : Signal.Address Action -> Model -> Html
viewAllContacts address model =
  let
    filteredContacts = List.filter (contactHasContent model.filterQuery.string) model.contacts
    --colorToContacts = List.map (\cat -> (cat.color.string, contactsWithCategory filteredContacts cat.id)) model.categories |> Dict.fromList
    --colorsToHTML = Dict.map (\color -> \conts -> ul [class "category_wrapper", style [("border-color", color)]] ) colorToContacts
    contactsHtml = List.map (contactWithCategory model >> viewForContact address) filteredContacts


  in
    div [class "container"]
      [ h3 [] [ text ("All contacts") ]
      , div [class "filter_field"] [ filterField address model ]
      , div [class "actions"] [ indexButton address ]
      , hr [] []
      , ul [class "category_wrapper"] contactsHtml
      ]

viewForContact : Signal.Address Action -> Contact -> Html
viewForContact address contact =
  let
    name = contact.name.string
    nameField = field defaultStyle (Signal.message (Signal.forwardTo address (ModifyContactName contact.id))) "Name" contact.name

    company = contact.company.string
    companyField = field defaultStyle (Signal.message (Signal.forwardTo address (ModifyContactCompany contact.id))) "Name" contact.company

    birthdayField = field defaultStyle (Signal.message (Signal.forwardTo address (ModifyContactBirthday contact.id))) "Birthday" contact.birthday


    buildDL =
      List.map (\ (name, value) -> li [] [h4 [] [text(name)], div [class "info"] [value]])

    contentViewMapper remAddress content remText =
       viewForContactContent address content (button [onClick remAddress] [text remText])

    addresses = List.map (\content -> contentViewMapper (RemoveAddress contact.id content.id) content "Remove Address") contact.addresses
    phones = List.map (\content -> contentViewMapper (RemoveAddress contact.id content.id) content "Remove Address") contact.phones
    mails = List.map (\content -> contentViewMapper (RemoveAddress contact.id content.id) content "Remove Address") contact.emails

    deleteButton = button [onClick address (RemoveContact contact.id)] [ text "Delete" ]

    dL =
      [ ("Name", fromElement nameField)
      , ("Company", fromElement companyField)
      , ("Birthday", fromElement birthdayField)
      , ("Addresses", ul [] addresses)
      , ("Phone numbers", ul [] phones)
      , ("E-Mails", ul [] mails)
      , ("Actions", span [] [deleteButton])
      ]

    dLC = case contact.categoryObject of
      Just cat -> (List.append dL [("Category",  p [] [span [class "color", style [("background-color", cat.color.string)]] [], text cat.name.string])])
      Nothing -> dL

  in
    li [] [ ul [class "contact"] (buildDL dLC) ]

viewForContactContent : Signal.Address Action -> ContactContent -> Html -> Html
viewForContactContent address content removeButton =
  li []
    [ fromElement (field defaultStyle (Signal.message (Signal.forwardTo address (ModifyContactContent content))) "content" content.text)
    , removeButton
    ]


-- STYLES

listStyle : String -> Attribute
listStyle color =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("color", color)
    , ("background-color", "black")
    ]

-- HELPERS

queryUpdateMessage : Signal.Address Action -> Content -> Signal.Message
queryUpdateMessage address content =
  Signal.message address (Filter content)

filterField : Signal.Address Action -> Model -> Html
filterField address model =
  field defaultStyle (queryUpdateMessage address) "Search" model.filterQuery |> fromElement

indexButton : Signal.Address Action -> Html
indexButton address = button [onClick address ShowIndex] [ text "Index"]

contactWithCategory : Model -> Contact -> Contact
contactWithCategory model contact =
  let
    category = List.head (List.filter (\cat -> cat.id == contact.category) model.categories)
  in
    {contact | categoryObject = category}

contactsWithCategory : List Contact -> ID -> List Contact
contactsWithCategory contacts categoryID =
  List.filter (\contact -> contact.category == categoryID) contacts

categoryHasContent : String -> Category -> Bool
categoryHasContent query category =
  stringsHaveContent query
    [ category.name.string
    , category.color.string
    ]

contactHasContent : String -> Contact -> Bool
contactHasContent query contact =
  let
    contentContactFilter content =
      stringHasContent query content.text.string

  in
    stringsHaveContent query
      [ contact.name.string
      , contact.company.string
      , contact.birthday.string]
    || List.any contentContactFilter contact.addresses
    || List.any contentContactFilter contact.emails
    || List.any contentContactFilter contact.phones

stringsHaveContent : String -> List String -> Bool
stringsHaveContent query strings = List.any (stringHasContent query) strings

stringHasContent : String -> String -> Bool
stringHasContent query string =
  String.contains (String.toLower query) (String.toLower string)

-- IMPORT

getContacts : Effects Action
getContacts =
  Http.get contactBookDecoder importUrl
    |> Task.toMaybe
    |> Task.map ProcessImport
    |> Effects.task

type alias ContactImportType =
  (String, String, List String, List String, List String, String, ID)

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
    ( { newModel
        | categories = newCategories
        , nextCategoryID = catMaxId + 1
        , contacts = newContacts
        , nextContactID = newNextContactID
      }
    , newAction
    )

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
  Decode.object7 (,,,,,,)
    ("name" := Decode.string)
    ("company" := Decode.string)
    ("addresses" := Decode.list Decode.string)
    ("phones" := Decode.list Decode.string)
    ("emails" := Decode.list Decode.string)
    ("dateOfBirth" := Decode.string)
    ("category" := Decode.int)
