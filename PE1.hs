module PE1 where

import Text.Printf

-- PE1: Recipe Calculator
-- The premise of this homework if to write a recipe calculator that
-- calculates: how much a recipe costs to make, what can be made with the
-- ingredients already available, and how much extra ingredients need to
-- be bought in order to make a recipe.

-- Recipe = Recipe Name [(Ingredient, Quantity)]
data Recipe = Recipe String [(String, Double)] deriving Show

-- Price = Price Ingredient Quantity Price
data Price = Price String Double Double deriving Show

-- You can use this as-is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- Calculate how much the given amount of the given ingredient costs
getIngredientCost :: (String, Double) -> [Price] -> Double
getNameFromList (Price name amount price) = name
getPricePerGram (Price name amount price) = price/amount
findPricePerGram name list = [x | x<-list, getNameFromList x == name] !! 0
getIngredientCost (name, amount) list = getRounded (amount * getPricePerGram (findPricePerGram name list))

-- Calculate how much it costs to buy all the ingredients of a recipe
recipeCost :: Recipe -> [Price] -> Double
getIngredients (Recipe name ingredients) = ingredients
recipeCost recipe list = getRounded (sum[getIngredientCost x list | x<-getIngredients recipe])

-- Given a list of how much you already have of each ingredient,
-- calculate how much of which ingredients are missing for a recipe
missingIngredients :: Recipe -> [(String, Double)] -> [(String, Double)]
getAmount name list = if [snd x | x<-list, fst x == name] == [] then 0 else [getRounded(snd x) | x<-list, fst x == name] !! 0
missingIngredients recipe list = [(fst x, getRounded (snd x - getAmount (fst x) list)) | x<-getIngredients recipe, getAmount (fst x) list < snd x]


-- Given a list of ingredients in your kitchen, calculate what you would
-- have left after making the given recipe. If there isn't enough of an
-- ingredient, the recipe cannot be made! You shouldn't change the amount
-- of ingredient in that case.
makeRecipe :: [(String, Double)] -> Recipe -> [(String, Double)]
makeRecipe list recipe = if missingIngredients recipe list /= [] then list else [(fst x, getRounded(snd x - getAmount (fst x) (getIngredients recipe))) | x<-list, getRounded(snd x) /= getRounded(getAmount (fst x) (getIngredients recipe))]

-- Given a list of ingredients you already have, and a list of recipes,
-- make a shopping list showing how much of each ingredient you need
-- to buy, and its cost. Each ingredient mush appear in the shopping list
-- at most once (no duplicates!).
makeShoppingList :: [(String, Double)] -> [Recipe] -> [Price] -> [(String, Double, Double)]
getIngredientAmount name recipe = sum [snd x|x<-recipe, fst x == name]
allRecipes recipe = concat[getIngredients x|x<-recipe]
makeList price = [(getNameFromList x, 0)|x<-price]
addRecipes list recipe = [(fst x, getIngredientAmount (fst x) recipe)|x<-list]
difference recipes stock = [(fst x, snd x - getIngredientAmount (fst x) stock)|x<-recipes, snd x - getIngredientAmount (fst x) stock>0]
makeShoppingList stock recipe price = [(fst x, getRounded(snd x), getRounded(getIngredientCost x price))| x<-difference (addRecipes (makeList price) (allRecipes recipe)) stock]
