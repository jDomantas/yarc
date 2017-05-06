'use strict';

window.factorioSolve = function(params) {
  // collect dependencies for each item, and recipes to make them
  var dependencies = {};
  var madeIn = {};
  var recipesByName = {};
  for (var i = 0; i < params.recipes.length; i++) {
    var recipe = params.recipes[i];
    recipesByName[recipe.rawName] = recipe;
    // if (recipe.results.length > 1)
    //   console.log(recipe.rawName);
    for (var j = 0; j < recipe.results.length; j++) {
      var output = recipe.results[j].item.rawName;
      if (!(output in dependencies))
        dependencies[output] = [];
      if (!(output in madeIn))
        madeIn[output] = [];
      madeIn[output].push(recipe);
      for (var k = 0; k < recipe.ingredients.length; k++) {
        var ingredient = recipe.ingredients[k].item.rawName;
        dependencies[output].push(ingredient);
        if (!(ingredient in dependencies))
          dependencies[ingredient] = [];
        if (!(ingredient in madeIn))
          madeIn[ingredient] = [];
      }
    }
  }


  // sort items
  var sortedItems = [];
  var visited = {};
  function toposort(item) {
    if (visited[item])
      return;
    for (var i = 0; i < dependencies[item].length; i++)
      toposort(dependencies[item][i]);
    sortedItems.push(item);
    visited[item] = true;
  }

  for (var item in dependencies)
    toposort(item);


  // create initial item bag
  var recipeUsage = {};
  var itemBag = {};
  for (var i = 0; i < params.requests.length; i++) {
    var requested = params.requests[i].item.rawName;
    var amount = params.requests[i].amount;
    if (amount > 0) {
      if (requested in itemBag)
        itemBag[requested] += amount;
      else
        itemBag[requested] = amount;
    }
  }


  // calculate how to make all items that appear in exactly one recipe
  for (var i = sortedItems.length - 1; i >= 0; i--) {
    var item = sortedItems[i];
    // if (madeIn[item].length > 1) {
    //   console.log('multiple ways to make ' + item);
    //   console.log(madeIn[item]);
    // }
    if (!(item in itemBag))
      continue;
    if (madeIn[item].length != 1)
      continue;
    var recipe = madeIn[item][0];
    var timesCompleted = itemBag[item] / recipe.results[0].amount;
    recipeUsage[recipe.rawName] = timesCompleted;
    for (var j = 0; j < recipe.ingredients.length; j++) {
      var ingr = recipe.ingredients[j];
      var name = ingr.item.rawName;
      if (name in itemBag)
        itemBag[name] += ingr.amount * timesCompleted;
      else
        itemBag[name] = ingr.amount * timesCompleted;
    }
    delete itemBag[item];
  }

  // console.log(recipeUsage);
  // console.log(itemBag);

  // calculate how to make oil products
  var lightOil = ('light-oil' in itemBag) ? itemBag['light-oil'] : 0;
  var heavyOil = ('heavy-oil' in itemBag) ? itemBag['heavy-oil'] : 0;
  var petroleumGas = ('petroleum-gas' in itemBag) ? itemBag['petroleum-gas'] : 0;
  var solidFuel = ('solid-fuel' in itemBag) ? itemBag['solid-fuel'] : 0;

  if (heavyOil > 0 || lightOil > 0 || solidFuel > 0 || petroleumGas > 0) {
    var usedCoal = ('coal' in itemBag) ? itemBag['coal'] : 0;
    var usedOil = ('crude-oil' in itemBag) ? itemBag['crude-oil'] : 0;
    
    function outputAmount(recipe, item) {
      for (var i = 0; i < recipe.results.length; i++) {
        if (recipe.results[i].item.rawName === item) {
          return recipe.results[i].amount;
        }
      }
      console.log('probably should not happen');
      return 0;
    }

    function inputAmount(recipe, item) {
      for (var i = 0; i < recipe.ingredients.length; i++) {
        if (recipe.ingredients[i].item.rawName === item) {
          return recipe.ingredients[i].amount;
        }
      }
      console.log('probably should not happen (2)');
      return 0;
    }

    var coalToOil = recipesByName['coal-liquefaction'];
    var coalRequired = inputAmount(coalToOil, 'coal');

    var oilProcessing = recipesByName['advanced-oil-processing'];
    var oilRequired = inputAmount(oilProcessing, 'crude-oil');

    function tryMakeFrom(coal, oil) {
      var recipes = {};
      var itemBag = {
        'heavy-oil' : 0,
        'light-oil' : 0,
        'solid-fuel' : 0,
        'petroleum-gas' : 0,
        'coal' : 0,
        'crude-oil' : 0,
        'water' : 0,
      };
      function doRecipe(recipe, times) {
        recipes[recipe.rawName] = times;
        for (var i = 0; i < recipe.results.length; i++)
          itemBag[recipe.results[i].item.rawName] += recipe.results[i].amount * times;
        for (var i = 0; i < recipe.ingredients.length; i++)
          itemBag[recipe.ingredients[i].item.rawName] -= recipe.ingredients[i].amount * times;
      }
      // convert coal and crude oil to oil products
      if (coal > 0) {
        var times = (coal / coalRequired);
        doRecipe(coalToOil, coal / coalRequired);
      }
      if (oil > 0) {
        var times = (oil / oilRequired);
        doRecipe(oilProcessing, oil / oilRequired);
      }
      // we need to have enough heavy oil without further processing
      if ((itemBag['heavy-oil'] = itemBag['heavy-oil'] - heavyOil) < 0) return null;
      // crack unused heavy oil to light oil
      if (itemBag['heavy-oil'] > 0) {
        var cracking = recipesByName['heavy-oil-cracking'];
        doRecipe(cracking, itemBag['heavy-oil'] / inputAmount(cracking, 'heavy-oil'));
      }
      // we need to have enough light oil without further processing
      if ((itemBag['light-oil'] = itemBag['light-oil'] - lightOil) < 0) return null;
      // convert light oil to solid fuel
      var missingFuel = solidFuel;
      if (missingFuel > 0) {
        var toFuel = recipesByName['solid-fuel-from-light-oil'];
        var maxFromInputs = itemBag['light-oil'] / inputAmount(toFuel, 'light-oil');
        var maxFromOutputs = solidFuel / outputAmount(toFuel, 'solid-fuel');
        var times = Math.min(maxFromInputs, maxFromOutputs);
        missingFuel -= times * outputAmount(toFuel, 'solid-fuel');
        doRecipe(toFuel, times);
      }
      // crack remaining light oil to petroleum gas if we need extra gas
      var missingPetrol = Math.max(petroleumGas - itemBag['petroleum-gas'], 0);
      if (itemBag['light-oil'] > 0 && missingPetrol > 0) {
        var cracking = recipesByName['light-oil-cracking'];
        var maxFromInputs = itemBag['light-oil'] / inputAmount(cracking, 'light-oil');
        var maxFromOutputs = missingPetrol / outputAmount(cracking, 'petroleum-gas');
        var times = Math.min(maxFromInputs, maxFromOutputs);
        doRecipe(cracking, times);
      }
      // we need to have enough petroleum gas without further processing
      if ((itemBag['petroleum-gas'] = itemBag['petroleum-gas'] - petroleumGas) < 0) return null;
      // convert unused petroleum gas to solid fuel if we need it
      if (missingFuel > 0) {
        var toFuel = recipesByName['solid-fuel-from-petroleum-gas'];
        var maxFromInputs = itemBag['petroleum-gas'] / inputAmount(toFuel, 'petroleum-gas');
        var maxFromOutputs = missingFuel / outputAmount(toFuel, 'solid-fuel');
        var times = Math.min(maxFromInputs, maxFromOutputs);
        doRecipe(toFuel, times);
      }
      // we need to have enough solid fuel
      if ((itemBag['solid-fuel'] = itemBag['solid-fuel'] - solidFuel) < 0) return null;
      // everything is ok
      return recipes;
    }

    // binary search: find lowest k such that we can make oil products out of:
    //   coal: max(0, k * params.coal - alreadyUsedCoal)
    //   oil: max(0, k * params.oil - alreadyUsedOil)
    var lowBound = 0;
    var highBound = 1e10;
    var bestResult = null;
    var paramSum = (params.coal + params.oil);
    if (paramSum < 1e-20) {
      params.coal = params.oil = 1;
    } else {
      params.coal /= paramSum;
      params.oil /= paramSum;
    }
    for (var i = 0; i < 100; i++) {
      var mid = (lowBound + highBound) / 2;
      var coal = Math.max(0, mid * params.coal - usedCoal);
      var oil = Math.max(mid * params.oil - usedOil);

      var result = tryMakeFrom(coal, oil);
      if (result !== null) {
        bestResult = result;
        highBound = mid;
      } else {
        lowBound = mid;
      }
    }

    for (var key in bestResult) {
      if (key in recipeUsage) {
        recipeUsage[key] += bestResult[key];
      } else {
        recipeUsage[key] = bestResult[key];
      }
    }
  }

  var results = [];
  for (var recipe in recipeUsage) {
    if (recipeUsage[recipe] > 1e-10) {
      results.push([recipe, recipeUsage[recipe]]);
    }
  }

  return results;
};