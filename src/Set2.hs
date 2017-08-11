{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ (show a)

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay key ((lk, lv):xs)
  | key == lk = Just lv
  | otherwise = lookupMay key xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay (x:xs) = Just $ foldl (\a b -> if a > b then a else b) x xs 

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = Just x
minimumMay (x:xs) = Just $ foldl (\a b -> if a < b then a else b) x xs 

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f Nothing = Nothing
chain f (Just v)  = f v

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

-- Ugly
queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd key = 
  case lookupMay key gd of
      Nothing -> Nothing
      Just xs -> case tailMay xs of
          Nothing -> Nothing
          Just tail -> case maximumMay tail of
              Nothing -> Nothing
              Just max -> case headMay xs of
                  Nothing -> Nothing
                  Just head -> divMay (fromIntegral max) (fromIntegral head)

-- Pretty much bind with no "do" syntax sugar
queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gd key =
  link (lookupMay key gd)               $ 
    \ls    -> link (tailMay ls)         $
    \tail  -> link (maximumMay tail)    $
    \max   -> link (headMay ls)         $
    \head  -> divMay (fromIntegral max) (fromIntegral head)

yLink :: Maybe a -> Maybe b -> (a -> b -> Maybe c) -> Maybe c
yLink a b f = link a (\a -> link b (f a))

{-
Another option, but tutorial said to use link

yLink Nothing _ _ = Nothing
yLink _ Nothing _ = Nothing
yLink (Just a) (Just b) f = f a b
-}

--addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
--addSalaries salaries personA personB = 