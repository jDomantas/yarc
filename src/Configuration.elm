module Configuration exposing
  ( Model, Msg
  , init, update, view
  , makeConcreteRecipes, getRatio, getTime
  )

import Html exposing (Html)
import Html.Attributes as Attrib
import Html.Events as Events
import Set
import ListHelp
import Specializations as Spec exposing (Specializations)
import Model exposing (..)
import Factorio


type alias Model =
  { specializations : Specializations
  , prodModule : Maybe Module
  , speedModule : Maybe Module
  , coalAmount : String
  , oilAmount : String
  , time : String
  , priority : AssemblerPriority
  }


type Msg
  = SpecMsg Spec.Msg
  | SetProdModule (Maybe Module)
  | SetSpeedModule (Maybe Module)
  | SetCoalAmount String
  | SetOilAmount String
  | SetTime String
  | SetPriority AssemblerPriority


init : Model
init =
  { specializations = []
  , prodModule = Nothing
  , speedModule = Nothing
  , coalAmount = "0"
  , oilAmount = "1"
  , time = "60"
  , priority = Best
  }


update : FactorioData -> Msg -> Model -> Model
update data msg model =
  case msg of
    SpecMsg msg ->
      { model | specializations = Spec.update data msg model.specializations }

    SetProdModule mod ->
      { model | prodModule = mod }

    SetSpeedModule mod ->
      { model | speedModule = mod }

    SetCoalAmount amount ->
      { model | coalAmount = amount }
    
    SetOilAmount amount ->
      { model | oilAmount = amount }
    
    SetTime time ->
      { model | time = time }
    
    SetPriority priority ->
      { model | priority = priority }


makeConcreteRecipes : FactorioData -> Model -> List ConcreteRecipe
makeConcreteRecipes data model =
  let
    specialized =
      model.specializations
      |> List.map (.recipe >> .rawName)
      |> Set.fromList

    toSpecialization recipe =
      let
        assembler = Factorio.pickAssembler data model.priority recipe
        
        modules =
          [ model.prodModule, model.speedModule ]
          |> ListHelp.flatten
          |> List.filter (flip Factorio.canBeUsed recipe)
          |> List.head
          |> List.repeat (assembler.moduleSlots)
      in
        { recipe = recipe
        , assembler = assembler
        , modules = modules
        }
  in
    data.recipes
    |> List.filter (\r -> not (Set.member r.rawName specialized))
    |> List.map toSpecialization
    |> List.append model.specializations
    |> List.map Spec.toConcreteRecipe


getRatio : Model -> Result String (Float, Float)
getRatio model =
  let
    coal = 
      model.coalAmount
      |> String.toFloat
      |> Result.mapError (always <| "Bad coal amount: " ++ model.coalAmount)
    
    oil =
      model.oilAmount
      |> String.toFloat
      |> Result.mapError (always <| "Bad oil amount: " ++ model.oilAmount)
  in
    case (coal, oil) of
      (Ok a, Ok b) ->
        Ok (a, b)
      
      (Err e, _) ->
        Err e
      
      (_, Err e) ->
        Err e


getTime : Model -> Result String Float
getTime model =
  model.time
  |> String.toFloat
  |> Result.mapError (always <| "Bad time: " ++ model.time)


view : FactorioData -> Model -> Html Msg
view data model =
  Html.div
    [ Attrib.class "config" ]
    [ Html.h3 [] [ Html.text "Configuration" ]
    , Html.table []
      [ labeled "Productivity module" (viewProdModuleSelect data model)
      , labeled "Speed module" (viewSpeedModuleSelect data model)
      , labeled "Assembler priority" (viewAssemblerPrioritySelect model)
      , labeled "Wanted coal amount" (viewCoalAmountSelect model)
      , labeled "Wanted oil amount" (viewOilAmountSelect model)
      , labeled "Time (seconds)" (viewTimeSelect model)
      ]
    , Html.h3 [] [ Html.text "Customized recipes" ]
    , Html.map SpecMsg (Spec.view data model.specializations)
    ]


labeled : String -> Html a -> Html a
labeled label html =
  Html.tr []
    [ Html.td [] [ Html.text label ]
    , Html.td [] [ html ]
    ]


viewProdModuleSelect : FactorioData -> Model -> Html Msg
viewProdModuleSelect data model =
  let
    selected value =
      data.modules
      |> ListHelp.find (\mod -> mod.item.rawName == value)
      |> SetProdModule

    selectedModule =
      model.prodModule
      |> Maybe.map (.item >> .name)
      |> Maybe.withDefault "none"

    moduleOption mod =
      if mod.productivityBonus > 0 then
        Just <| Html.option
          [ Attrib.selected (mod.item.rawName == selectedModule)
          , Attrib.value mod.item.rawName
          ]
          [ Html.text mod.item.name ]
      else
        Nothing

    defaultOption =
      Html.option
        [ Attrib.selected (selectedModule == "none") ]
        [ Html.text "None" ]
  in
    Html.select
      [ Events.onInput selected
      , Attrib.class "selectProdModule"
      ]
      (defaultOption :: List.filterMap moduleOption data.modules)


viewSpeedModuleSelect : FactorioData -> Model -> Html Msg
viewSpeedModuleSelect data model =
  let
    selected value =
      data.modules
      |> ListHelp.find (\mod -> mod.item.rawName == value)
      |> SetSpeedModule

    selectedModule =
      model.speedModule
      |> Maybe.map (.item >> .name)
      |> Maybe.withDefault "none"

    moduleOption mod =
      if mod.speedBonus > 0 then
        Just <| Html.option
          [ Attrib.selected (mod.item.rawName == selectedModule)
          , Attrib.value mod.item.rawName
          ]
          [ Html.text mod.item.name ]
      else
        Nothing

    defaultOption =
      Html.option
        [ Attrib.selected (selectedModule == "none") ]
        [ Html.text "None" ]
  in
    Html.select
      [ Events.onInput selected
      , Attrib.class "selectSpeedModule"
      ]
      (defaultOption :: List.filterMap moduleOption data.modules)


viewAssemblerPrioritySelect : Model -> Html Msg
viewAssemblerPrioritySelect model =
  let
    selected value =
      if value == "Best" then
        SetPriority Best
      else
        SetPriority Cheapest

    option value =
      Html.option
        [ Attrib.value value ]
        [ Html.text value ]
  in
    Html.select
      [ Events.onInput selected
      , Attrib.class "selectPriority"
      ]
      [ option "Best"
      , option "Cheapest"
      ]


viewCoalAmountSelect : Model -> Html Msg
viewCoalAmountSelect model =
  Html.input
    [ Attrib.value model.coalAmount
    , Attrib.class "selectRatio numericInput"
    , Events.onInput SetCoalAmount
    ]
    []


viewOilAmountSelect : Model -> Html Msg
viewOilAmountSelect model =
  Html.input
    [ Attrib.value model.oilAmount
    , Attrib.class "selectRatio numericInput"
    , Events.onInput SetOilAmount
    ]
    []


viewTimeSelect : Model -> Html Msg
viewTimeSelect model =
  Html.input
    [ Attrib.value model.time
    , Attrib.class "selectTime numericInput"
    , Events.onInput SetTime
    ]
    []
