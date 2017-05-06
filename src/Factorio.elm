module Factorio exposing (..)

import Set
import ListHelp
import Model exposing (..)


canCraft : Assembler -> Recipe -> Bool
canCraft assembler recipe =
  (List.length recipe.ingredients <= assembler.maxInputs) &&
  (Set.member recipe.category assembler.categories)


canBeUsed : Module -> Recipe -> Bool
canBeUsed mod recipe =
  case mod.limitations of
    LimitedTo recipes ->
      Set.member recipe.rawName recipes

    None ->
      True


pickAssembler : FactorioData -> AssemblerPriority -> Recipe -> Assembler
pickAssembler data priority recipe =
  let
    available =
      data.assemblers
      |> List.filter (flip canCraft recipe)
      |> List.sortBy (\a -> (a.speed, a.moduleSlots))

    -- let's assume that best assembler is fastest, and cheapest is slowest
    assembler = case priority of
      Cheapest ->
        List.head available
      
      Best ->
        ListHelp.last available
  in
    case assembler of
      Just assembler ->
        assembler

      Nothing ->
        Debug.crash <| "nothing can be used for recipe " ++ recipe.name
