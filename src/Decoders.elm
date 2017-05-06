module Decoders exposing (factorioData)

import Set
import Regex
import Json.Decode exposing (Decoder, list, string, int, float, nullable, map)
import Json.Decode.Pipeline exposing (decode, required)
import Model exposing (..)


item : Decoder Item
item =
  decode Item
  |> required "name" string
  |> required "rawName" string


stack : Decoder Stack
stack =
  decode Stack
  |> required "item" item
  |> required "amount" float


recipe : Decoder Recipe
recipe =
  decode Recipe
  |> required "name" string
  |> required "rawName" string
  |> required "results" (list stack)
  |> required "ingredients" (list stack)
  |> required "time" float
  |> required "category" string


assembler : Decoder Assembler
assembler =
  decode Assembler
  |> required "item" item
  |> required "moduleSlots" int
  |> required "maxInputs" int
  |> required "categories" (list string |> map Set.fromList)
  |> required "speed" float


limitations : Decoder Limitations
limitations =
  nullable (list string)
  |> map (\maybeList -> case maybeList of
    Just list ->
      LimitedTo (Set.fromList list)

    Nothing ->
      None)


mod : Decoder Module
mod =
  decode Module
  |> required "item" item
  |> required "speedBonus" float
  |> required "prodBonus" float
  |> required "limitations" limitations


factorioData : Decoder FactorioData
factorioData =
  let
    fixRecipes recipes =
      recipes
      |> List.filter keepRecipe
      |> List.map normalizeRecipe
  in
    decode FactorioData
    |> required "items" (list item |> map (List.sortBy .name))
    |> required "recipes" (list recipe |> map (fixRecipes >> List.sortBy .name))
    |> required "assemblers" (list assembler |> map (List.sortBy (.item >> .name)))
    |> required "modules" (list mod |> map (List.sortBy (.item >> .name)))


keepRecipe : Recipe -> Bool
keepRecipe recipe =
  let
    regex = Regex.regex "^(fill|empty)-[a-z\\-]+-barrel$"

    badRecipes =
      [ "kovarex-enrichment-process"
      , "nuclear-fuel-reprocessing"
      , "basic-oil-processing"
      , "solid-fuel-from-heavy-oil"
      , "uranium-processing"
      ]
  in
    not (Regex.contains regex recipe.rawName || List.member recipe.rawName badRecipes)


normalizeRecipe : Recipe -> Recipe
normalizeRecipe recipe =
  let
    removeFromList : Stack -> List Stack -> (Float, List Stack)
    removeFromList stack list =
      case list of
        [] ->
          (0, [])

        x :: xs ->
          if x.item == stack.item && x.amount >= stack.amount then
            (stack.amount, { x | amount = x.amount - stack.amount } :: xs)
          else if x.item == stack.item then
            (x.amount, xs)
          else
            let
              (removed, newList) = removeFromList stack xs
            in
              (removed, x :: newList)

    fix : Stack -> (List Stack, List Stack) -> (List Stack, List Stack)
    fix stack (acc, results) =
      let
        (removedAmount, newResults) = removeFromList stack results

        newStack =
          { stack | amount = stack.amount - removedAmount }
      in
        if newStack.amount > 0 then
          (newStack :: acc, newResults)
        else
          (acc, newResults)

    (ingredients, results) = List.foldl fix ([], recipe.results) recipe.ingredients
  
  in
    { recipe
      | ingredients = ingredients
      , results = results
      }
