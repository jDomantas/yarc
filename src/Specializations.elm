module Specializations exposing
  ( Specializations, Msg
  , update, toConcreteRecipe, view
  )

import Html exposing (Html)
import Html.Attributes as Attrib
import Html.Events as Events
import ListHelp
import Model exposing (..)
import Factorio


type alias Specialization =
  { recipe : Recipe
  , assembler : Assembler
  , modules : List (Maybe Module)
  }


type alias Specializations = List Specialization


type Msg
  = NoOp
  | AddRecipe Recipe
  | RemoveRecipe Int
  | SetAssembler Int Assembler
  | SetModule Int Int (Maybe Module)


update : FactorioData -> Msg -> Specializations -> Specializations
update data msg spec =
  case msg of
    NoOp ->
      spec

    AddRecipe recipe ->
      let
        assembler = Factorio.pickAssembler data Best recipe
      in
        spec ++ [ {
          recipe = recipe,
          assembler = assembler,
          modules = List.repeat assembler.moduleSlots Nothing
        } ]

    RemoveRecipe index ->
      ListHelp.removeAt index spec

    SetAssembler index assembler ->
      let
        update s =
          { s
            | assembler = assembler
            , modules = ListHelp.setLength assembler.moduleSlots s.modules
            }
      in
        ListHelp.mapAt index update spec

    SetModule index slot mod ->
      let
        newModules s =
          ListHelp.mapAt slot (always mod) s.modules
      in
        ListHelp.mapAt index (\s -> { s | modules = newModules s }) spec


toConcreteRecipe : Specialization -> ConcreteRecipe
toConcreteRecipe spec =
  let
    prodBonus =
      spec.modules
      |> ListHelp.flatten
      |> List.map .productivityBonus
      |> List.sum

    speedBonus =
      spec.modules
      |> ListHelp.flatten
      |> List.map .speedBonus
      |> List.sum
    
    craftingTime =
      spec.recipe.time / (spec.assembler.speed + speedBonus)

    updatedResults =
      spec.recipe.results
      |> List.map (\stack -> { stack | amount = stack.amount * (1 + prodBonus) })

    updatedRecipe =
      let
        --  >:(
        recipe = spec.recipe
      in
        { recipe
          | time = craftingTime
          , results = updatedResults
          }
  in
    { spec
      | recipe = updatedRecipe
      , modules = List.filterMap identity spec.modules
      }


view : FactorioData -> Specializations -> Html Msg
view data spec =
  Html.div 
    [ Attrib.id "specList" ]
    ((List.indexedMap (viewSpecialization data) spec) ++ [viewAddNew data spec])


viewAddNew : FactorioData -> Specializations -> Html Msg
viewAddNew data spec =
  let
    isSpecialized recipe =
      List.any (\s -> s.recipe == recipe) spec

    recipeOption recipe =
      if isSpecialized recipe then
        Nothing
      else
        Just <| Html.option
          [ Attrib.value recipe.rawName
          , Attrib.selected False
          ]
          [ Html.text recipe.name ]

    onChange = Events.onInput <| \value ->
      case ListHelp.find (\recipe -> recipe.rawName == value) data.recipes of
        Just recipe ->
          AddRecipe recipe

        Nothing ->
          NoOp

    defaultOption =
      Html.option
        [ Attrib.value "-- select recipe --"
        , Attrib.selected True
        ]
        [ Html.text "-- select recipe --" ]
  in
    Html.select
      [ onChange
      , Attrib.id "addSpec"
      ]
      (defaultOption :: List.filterMap recipeOption data.recipes)


viewSpecialization : FactorioData -> Int -> Specialization -> Html Msg
viewSpecialization data index spec =
  Html.div 
    [ Attrib.class "spec" ]
    [ Html.p [ Attrib.class "specTitle" ] [ Html.text spec.recipe.name ]
    , viewAssemblerSelection data index spec
    , Html.div [ Attrib.class "specModuleList" ]
      (Html.p [ Attrib.class "specModuleListTitle" ] [ Html.text "Modules" ] ::
      List.indexedMap (viewModuleSelection data index spec) spec.modules)
    , Html.button
      [ Events.onClick (RemoveRecipe index)
      , Attrib.class "removeSpec"
      ]
      [ Html.text "Remove" ]
    ]


viewAssemblerSelection : FactorioData -> Int -> Specialization -> Html Msg
viewAssemblerSelection data index spec =
  let
    assemblerOption assembler =
      if Factorio.canCraft assembler spec.recipe then
        Just <| Html.option
          [ Attrib.value assembler.item.rawName
          , Attrib.selected (assembler.item.rawName == spec.assembler.item.rawName) ]
          [ Html.text assembler.item.name ]
      else
        Nothing

    onChange = Events.onInput <| \value ->
      case ListHelp.find (\assembler -> assembler.item.rawName == value) data.assemblers of
        Just assembler ->
          SetAssembler index assembler

        Nothing ->
          Debug.crash <| "assembler not defined: " ++ value
  in
    Html.select
      [ onChange
      , Attrib.class "selectAssembler"
      ]
      (List.filterMap assemblerOption data.assemblers)


viewModuleSelection : FactorioData -> Int -> Specialization -> Int -> (Maybe Module) -> Html Msg
viewModuleSelection data index spec slot current =
  let
    selectedModule = Maybe.map (.item >> .rawName) current

    moduleOption mod =
      if Factorio.canBeUsed mod spec.recipe then
        Just <| Html.option
          [ Attrib.value mod.item.rawName
          , Attrib.selected (selectedModule == Just mod.item.rawName) ]
          [ Html.text mod.item.name ]
      else
        Nothing

    onChange = Events.onInput <| \value ->
      case ListHelp.find (\mod -> mod.item.rawName == value) data.modules of
        Just mod ->
          SetModule index slot (Just mod)

        Nothing ->
          SetModule index slot Nothing

    noneOption =
      Html.option
        [ Attrib.value "None"
        , Attrib.selected (selectedModule == Nothing) ]
        [ Html.text "None" ]
  in
    Html.select
      [ onChange
      , Attrib.class "selectModule"
      ]
      (noneOption :: List.filterMap moduleOption data.modules)
