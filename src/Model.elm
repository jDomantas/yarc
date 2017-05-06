module Model exposing (..)

import Set exposing (Set)


type alias Item =
  { name : String
  , rawName : String
  }


type alias Stack =
  { item : Item
  , amount : Float
  }


type alias Recipe =
  { name : String
  , rawName : String
  , results : List Stack
  , ingredients : List Stack
  , time : Float
  , category : String
  }


type alias Assembler =
  { item : Item
  , moduleSlots : Int
  , maxInputs : Int
  , categories : Set String
  , speed : Float
  }


type Limitations
  = None
  | LimitedTo (Set String)


type alias Module =
  { item : Item
  , speedBonus : Float
  , productivityBonus : Float
  , limitations : Limitations
  }


type alias ConcreteRecipe =
  { recipe : Recipe
  , assembler : Assembler
  , modules : List Module
  }


type alias FactorioData =
  { items : List Item
  , recipes : List Recipe
  , assemblers : List Assembler
  , modules : List Module
  }


type AssemblerPriority
  = Best
  | Cheapest
