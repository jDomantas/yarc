import Html exposing (Html)
import Html.Attributes as Attrib
import Html.Events as Events
import Http
import ListHelp
import Model exposing (..)
import Decoders
import Configuration as Config
import Request
import Solver


type alias Model =
  { dataset : Dataset
  , status : Status
  }


type Status
  = Loading
  | Working Data
  | LoadError String


type alias Data =
  { data : FactorioData
  , config : Config.Model
  , requests : Request.Model
  , query : CurrentQuery
  }


type Msg
  = LoadData FactorioData
  | ChangeDataset Dataset
  | Error String
  | ConfigMsg Config.Msg
  | RequestMsg Request.Msg
  | RunQuery
  | Solution Plan


type alias PlanItem =
  { recipe : ConcreteRecipe
  , timesCompleted : Float
  , machinesRequired : Float
  , totalInputs : List Stack
  , totalOutputs : List Stack
  }


type alias Plan =
  { items : List PlanItem
  , resources : List Stack
  , byproducts : List Stack
  , timeTaken : Float
  }


type CurrentQuery
  = Empty
  | ParamError String
  | ShowingResult Plan
  | WaitingForAnswer Solver.Params


type Dataset
  = Factorio_0_15_9_normal


main : Program Never Model Msg
main = Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }


loadDataset : Dataset -> Cmd Msg
loadDataset dataset =
  let
    url = "/datasets/" ++ toString dataset ++ ".json"
  in
    Http.get url Decoders.factorioData
    |> Http.send (\result -> case result of
      Ok data ->
        LoadData data
        
      Err e ->
        Error (toString e))


init : (Model, Cmd Msg)
init =
  let
    initialDataset = Factorio_0_15_9_normal
  in
    (
      { dataset = initialDataset
      , status = Loading
      }
    , loadDataset initialDataset
    )


datasetName : Dataset -> String
datasetName dataset =
  case dataset of
    Factorio_0_15_9_normal ->
      "Factorio 0.15.9 (normal)"


makePlanItem : Float -> ConcreteRecipe -> Float -> PlanItem
makePlanItem time recipe timesCompleted =
  let
    scaleStacks amount =
      List.map (\stack -> { stack | amount = stack.amount * amount })
    
    machinesRequired = recipe.recipe.time * timesCompleted / time
  in
    { recipe = recipe
    , timesCompleted = timesCompleted
    , machinesRequired = machinesRequired
    , totalInputs = scaleStacks timesCompleted recipe.recipe.ingredients
    , totalOutputs = scaleStacks timesCompleted recipe.recipe.results
    }


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.status of
    Working data ->
      case data.query of
        WaitingForAnswer params ->
          let
            makeMsg solution =
              Solution
                { items =
                  solution.recipeUsage
                  |> List.map (uncurry (makePlanItem params.time))
                , resources = solution.resources
                , byproducts = solution.byproducts
                , timeTaken = params.time
                }
          in
            Solver.solutions params makeMsg

        _ ->
          Sub.none

    _ ->
      Sub.none


makeQueryParams : Data -> Result String Solver.Params
makeQueryParams data =
  let
    makeParams (coal, oil) time requests =
      { recipes = Config.makeConcreteRecipes data.data data.config
      , requests = requests
      , coal = coal
      , oil = oil
      , time = time
      }
  in
    Result.map3 makeParams
      (Config.getRatio data.config)
      (Config.getTime data.config)
      (Request.getRequests data.requests)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeDataset newDataset ->
      { dataset = newDataset
      , status = Loading
      } ! [ loadDataset newDataset ]

    LoadData data ->
      { model | status = Working
        { data = data
        , config = Config.init
        , requests = Request.init
        , query = Empty
        } } ! []

    Error error ->
      { model | status = LoadError error } ! []

    ConfigMsg msg ->
      case model.status of
        Working data ->
          { model | status = Working { data | config = Config.update data.data msg data.config } } ! []
        
        _ ->
          model ! []

    RequestMsg msg ->
      case model.status of
        Working data ->
          { model | status = Working { data | requests = Request.update msg data.requests } } ! []
        
        _ ->
          model ! []

    RunQuery ->
      case model.status of
        Working data ->
          case data.query of
            WaitingForAnswer _ ->
              model ! []

            _ ->
              case makeQueryParams data of
                Ok params ->
                  { model | status = Working { data | query = WaitingForAnswer params } }
                    ! [ Solver.runSolver params ]
                
                Err e ->
                  { model | status = Working { data | query = ParamError e } }
                    ! []
        
        _ ->
          model ! []

    Solution items ->
      case model.status of
        Working data ->
          { model | status = Working { data | query = ShowingResult items } } ! []
        
        _ ->
          model ! []


isCraftable : FactorioData -> Item -> Int
isCraftable data item =
  List.filter (\recipe -> List.any (\result -> result.item == item) recipe.results) data.recipes
  |> List.length


isUsed : FactorioData -> Item -> Bool
isUsed data item =
  List.any (\recipe -> List.any (\result -> result.item == item) recipe.ingredients) data.recipes


viewDatasetSelect : Model -> Html Msg
viewDatasetSelect model =
  let
    datasets =
      [ Factorio_0_15_9_normal
      ]

    datasetOption dataset =
      Html.option
        [ Attrib.selected (model.dataset == dataset)
        , Attrib.value (toString dataset)
        ]
        [ Html.text (datasetName dataset) ]
  in
    Html.select
      [ Attrib.id "datasetSelect"
      , Events.onInput <| \value ->
          case ListHelp.find (toString >> (==) value) datasets of
            Just dataset ->
              ChangeDataset dataset

            Nothing ->
              Debug.crash <| "Invalid value: " ++ value
      ]
      (List.map datasetOption datasets)


view : Model -> Html Msg
view model =
  let
    workingView =
      case model.status of
        Loading ->
          Html.p [] [ Html.text "Loading" ]

        LoadError e ->
          Html.p [] [ Html.text <| "Error: " ++ e ]

        Working data ->
          let
            configView =
              Config.view data.data data.config
              |> Html.map ConfigMsg

            requestsView =
              Request.view data.data data.requests
              |> Html.map RequestMsg

            isSolving =
              case data.query of
                WaitingForAnswer _ ->
                  True
                
                _ ->
                  False
          in
            Html.div []
              [ configView
              , requestsView
              , Html.button
                [ Attrib.id "solveButton"
                , Events.onClick RunQuery
                , Attrib.disabled isSolving
                ]
                [ Html.text "Solve" ]
              , viewQuery data.query
              ]
  in
    Html.div []
      [ viewDatasetSelect model
      , workingView
      ]


viewQuery : CurrentQuery -> Html a
viewQuery query =
  case query of
    Empty ->
      Html.text ""

    ParamError e ->
      Html.p
        [ Attrib.class "paramError" ]
        [ Html.text e ]

    ShowingResult plan ->
      Html.div
        [ Attrib.class "results" ]
        [ viewResources plan.timeTaken plan.resources
        , viewByproducts plan.timeTaken plan.byproducts
        , viewRecipes plan.timeTaken plan.items
        ]

    WaitingForAnswer _ ->
      Html.p
        [ Attrib.class "solvingStatus" ]
        [ Html.text "Calculating..." ]


viewResources : Float -> List Stack -> Html a
viewResources time stacks =
  Html.div
    [ Attrib.class "resources" ]
    [ Html.p
      [ Attrib.class "resourcesHeader "]
      [ Html.text "Consumed resources: " ]
    , viewItemList time stacks
    ]


viewByproducts : Float -> List Stack -> Html a
viewByproducts time stacks =
  if List.length stacks == 0 then
    Html.text ""
  else
    Html.div
      [ Attrib.class "byproducts" ]
      [ Html.p
        [ Attrib.class "byproductsHeader "]
        [ Html.text "Byproducts: " ]
      , viewItemList time stacks
      ]


viewItem : Stack -> Html a
viewItem stack =
  Html.span []
    [ viewNumber stack.amount
    , Html.text <| " × " ++ stack.item.name
    ]


viewItemWithSpeed : Float -> Stack -> Html a
viewItemWithSpeed time stack =
  let
    itemsPerSecond = stack.amount / time
  in
    Html.span []
      [ viewNumber stack.amount
      , Html.text <| " × " ++ stack.item.name ++ " ("
      , viewNumber itemsPerSecond
      , Html.text "/s)"
      ]


viewRecipes : Float -> List PlanItem -> Html a
viewRecipes time items =
  Html.div
    [ Attrib.class "recipes" ]
    (List.map (viewPlanItem time) items)


viewItemList : Float -> List Stack -> Html a
viewItemList time stacks =
  Html.ul [] (List.map (viewItemWithSpeed time >> List.singleton >> Html.li []) stacks)


viewPlanItem : Float -> PlanItem -> Html a
viewPlanItem time item =
  let
    viewMachine =
      let
        moduleCount =
          case List.length item.recipe.modules of
            0 ->
              "no modules"
            
            1 ->
              "1 module"

            n ->
              toString n ++ " modules"
      in
        Html.span []
          [ Html.text " ("
          , viewNumber item.machinesRequired
          , Html.text " × "
          , Html.text item.recipe.assembler.item.name
          , Html.text ", "
          , Html.text moduleCount
          , Html.text ")"
          ]

    listHeader class text =
      Html.p
        [ Attrib.class class ]
        [ Html.text text ]

    inputsHeader = listHeader "planItemInputsHeader" "Total input:"
    outputsHeader = listHeader "planItemOutputsHeader" "Total output:"
    modulesHeader = listHeader "planItemModulesHeader" "Modules:"

    viewModule mod =
      Html.li
        [ Attrib.class "module" ]
        [ Html.text mod.item.name ]

    viewModules =
      if List.length item.recipe.modules == 0 then
        []
      else
        [ modulesHeader
        , Html.ul [] (List.map viewModule item.recipe.modules)
        ]
  in
    Html.details
      [ Attrib.class "planItem" ]
      [ Html.summary
        [ Attrib.class "planItemTitle" ]
        [ Html.text <| "Recipe: " ++ item.recipe.recipe.name
        , viewMachine
        ]
      , Html.div
        [ Attrib.class "planItemModules" ]
        viewModules
      , Html.div
        [ Attrib.class "planItemInputs" ]
        [ inputsHeader
        , viewItemList time item.totalInputs
        ]
      , Html.div
        [ Attrib.class "planItemOutputs" ]
        [ outputsHeader
        , viewItemList time item.totalOutputs
        ]
      ]


viewNumber : Float -> Html a
viewNumber num =
  Html.span
    [ Attrib.class "number" ]
    [ Html.text <| toString (toFloat (round (num * 100000)) / 100000) ]
