# YARC (Yet Another Ratio Calculator)

YARC is a planner tool for Factorio. It can calculate how many resources are required to craft wanted items, and how many machines are required to achieve requested crafting speed.


## How to use

### Configuration options

- Productivity module: which productivity module to use by default on recipes that allow it.
- Speed module: if productivity module is not available or 'None' was selected, which speed module to use.
- Assembler priority: how to pick assembler for recipe. 'Best' will pick fastest, and if there are several with same speed, then the one with most module slots (eg. electric furnace instead of steel furnace). 'Cheapest' will pick slowest assembler.
- Wanted coal/oil amount: coal liquefaction and advanced oil processing recipes have same outputs, therefore it is possible to make oil products using only coal, or using only oil. Solver will try to find solution that has the closest ratio of coal/oil resource usage to the one given by those two fields.
- Time: in how many seconds should requested items be crafted. Does not affect resource ratios, only assembler counts.

### Customized recipes

Solver will pick assembler and module configurations using the settings given before. However, if you want you can override those settings for each recipe sepparately. You can add recipes, and for each one pick what assembler will be used, and how to fill it with modules.


## Current limitations

- Right now uranium processing is not fully supported (to be exact, kovarex enrichment process and nuclear fuel reprocessing recipes are missing).
- Solver never uses basic oil processing, even though it's not always optimal. For example, if you wanted to produce lubricant, basic oil processing would be better because it gives more heavy oil for same amount of crude oil.
