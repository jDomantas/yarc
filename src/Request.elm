module Request exposing
  ( Model, Msg
  , init, update, view
  , getRequests
  )

import Html exposing (Html)
import Html.Attributes as Attrib
import Html.Events as Events
import ListHelp
import Model exposing (..)


type alias RawRequest =
  { item : Item
  , amount : String
  }


type alias Model = List RawRequest


type Msg
  = NoOp
  | AddRequest Item
  | RemoveRequest Int
  | ModifyAmount Int String


init : Model
init = []


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model

    AddRequest item ->
      model ++ [ {
        item = item,
        amount = "1"
      } ]

    RemoveRequest index ->
      ListHelp.removeAt index model

    ModifyAmount index amount ->
      ListHelp.mapAt index (\r -> { r | amount = amount }) model


getRequests : Model -> Result String (List Stack)
getRequests model =
  let
    toStack request =
      case String.toFloat request.amount of
        Ok amount ->
          Ok { item = request.item, amount = amount }

        Err _ ->
          Err request.item.name
    
    results =
      model
      |> List.map toStack
      |> ListHelp.collectResults
  in
    case results of
      Ok stacks ->
        if List.length stacks > 0 then
          Ok stacks
        else
          Err "No items requested"

      Err badItems ->
        let
          items =
            badItems
            |> ListHelp.join ", "
            |> Maybe.withDefault "you shouldn't see this"
        in
          Err ("Some requested items have bad amounts: " ++ items)


view : FactorioData -> Model -> Html Msg
view data model =
  Html.div 
    [ Attrib.id "requests" ]
    ([ Html.h3 [] [ Html.text "Requested items" ]
    , viewRequests model
    -- for some reason property `Attrib.selected` does not
    -- work if nothing changes before <selected> node
    -- ¯\_(ツ)_/¯
    ] ++ (if List.length model % 2 == 0 then [ Html.div [] [] ] else []) ++
    [ viewAddRequest data model 
    ])


viewAddRequest : FactorioData -> Model -> Html Msg
viewAddRequest data model =
  let
    isRequested item =
      List.any (\r -> r.item == item) model

    itemOption item =
      if isRequested item then
        Nothing
      else
        Just <| Html.option
          [ Attrib.value item.rawName
          , Attrib.selected False
          ]
          [ Html.text <| String.concat
            [ item.name
            , " ("
            , item.rawName
            , ")"
            ]
          ]

    onChange = Events.onInput <| \value ->
      case ListHelp.find (\item -> item.rawName == value) data.items of
        Just item ->
          AddRequest item

        Nothing ->
          NoOp

    defaultOption =
      Html.option
        [ Attrib.value "-- add item --"
        , Attrib.selected <| Debug.log "here" True
        ]
        [ Html.text "-- add item --" ]
  in
    Html.select
      [ onChange
      , Attrib.id "addRequest"
      ]
      (defaultOption :: List.filterMap itemOption data.items)


viewRequests : List RawRequest -> Html Msg
viewRequests requests =
  Html.table
    [ Attrib.id "requestList" ]
    (List.indexedMap viewRequest requests)


viewRequest : Int -> RawRequest -> Html Msg
viewRequest index request =
  Html.tr []
    (List.map (List.singleton >> Html.td [])
      [ Html.text <| String.concat
        [ request.item.name
        , " ("
        , request.item.rawName
        , ")"
        ]
      , amountInput index request.amount
      , Html.button
        [ Events.onClick (RemoveRequest index)
        , Attrib.class "removeRequest"
        ]
        [ Html.text "Remove" ]
      ]
    )

amountInput : Int -> String -> Html Msg
amountInput index value =
  Html.input
    [ Attrib.value value
    , Attrib.class "requestAmount numericInput"
    , Events.onInput (ModifyAmount index)
    ]
    []
