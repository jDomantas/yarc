port module Solver exposing (Params, runSolver, solutions)

import Dict exposing (Dict)
import Platform.Sub as Sub
import Model exposing (..)


type alias Params =
  { recipes : List ConcreteRecipe
  , requests : List Stack
  , time : Float
  , coal : Float
  , oil : Float
  }


type alias Solution =
  { recipeUsage : List (ConcreteRecipe, Float)
  , byproducts : List Stack
  , resources : List Stack
  }


type alias JsParams =
  { recipes : List Recipe
  , requests : List Stack
  , time : Float
  , coal : Float
  , oil : Float
  }


port solve : JsParams -> Cmd msg
port rawSolutions : (List (String, Float) -> msg) -> Sub msg


runSolver : Params -> Cmd msg
runSolver params =
  solve { params | recipes = List.map .recipe params.recipes }


solutions : Params -> (Solution -> msg) -> Sub msg
solutions params makeMsg =
  rawSolutions (solutionFromRaw params >> makeMsg)


solutionFromRaw : Params -> List (String, Float) -> Solution
solutionFromRaw params rawRecipes =
  let
    recipes =
      params.recipes
      |> List.foldl (\r -> Dict.insert r.recipe.rawName r) Dict.empty

    toRecipe name =
      case Dict.get name recipes of
        Just recipe ->
          recipe
        
        Nothing ->
          Debug.crash ("unknown recipe: " ++ name)

    recipeUsage =
      List.map (\(name, amount) -> (toRecipe name, amount)) rawRecipes

    removeStack stack =
      Dict.update (stack.item.rawName, stack.item.name) (add -stack.amount)

    netChange =
      findNetChange recipeUsage
      |> \d -> List.foldl removeStack d params.requests
      |> Dict.toList

    resourcesConsumed =
      netChange
      |> List.filterMap (\((raw, name), amount) ->
        if amount < -0.0000001 then
          Just { item = { name = name, rawName = raw }, amount = -amount }
        else
          Nothing)
    
    byproducts =
      netChange
      |> List.filterMap (\((raw, name), amount) ->
        if amount > 0.0000001 then
          Just { item = { name = name, rawName = raw }, amount = amount }
        else
          Nothing)
  in
    { recipeUsage = recipeUsage
    , resources = resourcesConsumed
    , byproducts = byproducts
    }


findNetChange : List (ConcreteRecipe, Float) -> Dict (String, String) Float
findNetChange recipeUsage =
  let
    scaleStack amount stack =
      { stack | amount = amount * stack.amount }

    recipeChange recipe amount =
      (List.map (scaleStack amount) recipe.recipe.results)
      ++
      (List.map (scaleStack -amount) recipe.recipe.ingredients)

    addStack stack =
      Dict.update (stack.item.rawName, stack.item.name) (add stack.amount)
  in
    recipeUsage
    |> List.concatMap (uncurry recipeChange)
    |> List.foldl addStack Dict.empty


add : Float -> Maybe Float -> Maybe Float
add value to =
  case to of
    Just existing ->
      Just (existing + value)

    Nothing ->
      Just value
