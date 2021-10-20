-- | This module explores different ways to implement the "pairing fold".

module MonusWeightedSearch.Examples.PairingFold where

-- $setup
-- >>> :{
-- data Tree = Leaf | Tree :*: Tree
-- instance Show Tree where
--   show Leaf = "."
--   show (xs :*: ys) = "(" ++ show xs ++ "*" ++ show ys ++ ")"
-- :}

-- | The traditional definition of the pairing fold, as given in the original
-- paper.
--
-- >>> pairFold1 (:*:) (replicate 5 Leaf)
-- Just ((.*.)*((.*.)*.))
pairFold1 :: (a -> a -> a) -> [a] -> Maybe a
pairFold1 f []     = Nothing
pairFold1 f (x:xs) = Just (go x xs)
  where
    go x []          = x
    go x1 (x2:[])    = f x1 x2
    go x1 (x2:x3:xs) = f (f x1 x2) (go x3 xs)

-- A function that is identical to the one above, although implemented as a
-- fold.
pairFold2 :: (a -> a -> a) -> [a] -> Maybe a
pairFold2 c xs = foldr f id xs Nothing
  where
    f x  k Nothing   = k (Just x)
    f x2 k (Just x1) = Just (maybe (c x1 x2) (c (c x1 x2)) (k Nothing))

data Acc3 a
  = Acc3 (Maybe a) (Maybe a)
  deriving Foldable

-- This is a slightly different version to the one above.
pairFold3 :: (a -> a -> a) -> [a] -> Maybe a
pairFold3 c xs = foldr ((Just .) . (maybe <*> c)) Nothing (foldr f (Acc3 Nothing Nothing) xs)
  where
    f x (Acc3 Nothing    xs) = Acc3 (Just x) xs
    f x1 (Acc3 (Just x2) xs) = Acc3 Nothing (Just (maybe (c x1 x2) (c (c x1 x2)) xs))

-- data Tree a = Leaf a | Tree a :*: Tree a deriving (Eq, Ord, Show)

-- prop :: [Word] -> Bool
-- prop xs = pairFold1 (:*:) ys == pairFold3 (:*:) ys
--   where
--     ys = map Leaf xs
