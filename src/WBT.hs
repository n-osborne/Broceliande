module WBT
  ()
  where

import Data.Monoid
import qualified Data.Foldable as F

-- Data declaration and instance

data WBT k = Ewbt | Nwbt k Int (WBT k) (WBT k) deriving (Show)

instance Functor WBT where
  fmap f Ewbt = Ewbt
  fmap f (Nwbt k s l r) = Nwbt (f k) s (fmap f l) (fmap f r)

instance (Ord k) => Monoid (WBT k) where
  mempty = Ewbt
  mappend = wbtAppend

instance F.Foldable WBT where
  foldMap f Ewbt = mempty
  foldMap f (Nwbt k s l r) = F.foldMap f l `mappend`
                             f k `mappend`
                             F.foldMap f r

-- constants (magic numbers)

-- According to Hirai and Kazuhiko 2011
magicNumb1 = 3
magicNumb2 = 2

-- Simple Primitives

wbtIsEmpty :: WBT k -> Bool
wbtIsEmpty Ewbt = True
wbtIsEmpty (Nwbt _ _ _ _) = False

wbtIsNotEmpty :: WBT k -> Bool
wbtIsNotEmpty t = not $ wbtIsEmpty t

wbtSize :: WBT k -> Int
wbtSize Ewbt = 0
wbtSize (Nwbt _ s _ _) = s

wbtHeight :: WBT k -> Int
wbtHeight Ewbt = 0
wbtHeight (Nwbt _ _ l r) = 1 + max (wbtHeight l) (wbtHeight r)

-- | Leaf constructor
wbtLeaf :: k -> WBT k
wbtLeaf key = Nwbt key 1 Ewbt Ewbt

-- | Get the min in a Maybe
wbtMin :: WBT k -> Maybe k
wbtMin Ewbt = Nothing
wbtMin (Nwbt k s l r)
  | wbtIsEmpty l = Just k
  | otherwise = wbtMin l
  
-- | elem primitive
wbtElem :: (Ord k) => k -> WBT k -> Bool
wbtElem x Ewbt = False
wbtElem x (Nwbt k _ l r)
  | x == k = True
  | x < k = wbtElem x l
  | x > k = wbtElem x r

-- | Get the nth element in a Maybe, starting at 0 as in an array.
wbtNthElem :: Int -> WBT k -> Maybe k
wbtNthElem _ Ewbt = Nothing
wbtNthElem n (Nwbt k s l r)
  | n > s = Nothing
  | n < x = wbtNthElem n l
  | n == x = Just k
  | otherwise = wbtNthElem (n - x - 1) r
  where x = wbtSize l
  
-- Specific Primitives

-- | Predicate for well balanced WBF
wbtIsBalanced :: WBT k -> Bool
wbtIsBalanced Ewbt = True
wbtIsBalanced (Nwbt k s l r) = cond1 && cond2
  where cond1 = 1 + wbtSize l <= magicNumb1 * (1 + wbtSize r)
        cond2 = 1 + wbtSize r <= magicNumb1 * (1 + wbtSize l)

-- | Predicate for single rotation
needSingleR :: WBT k -> WBT k -> Bool
needSingleR l r = (1 + wbtSize l) < magicNumb2 * (1 + wbtSize r)

-- | Single left rotation
singleLR :: WBT k -> WBT k
singleLR (Nwbt k s l (Nwbt k' s' l' r')) = Nwbt k' s1 (Nwbt k s2 l l') r'
  where s2 = wbtSize l + wbtSize l' + 1
        s1 = s2 + wbtSize r' + 1

-- | Single right rotation
singleRR :: WBT k -> WBT k
singleRR (Nwbt k s (Nwbt k' s' l' r') r) = Nwbt k' s1 l' (Nwbt k s2 r' r)
  where s2 = 1 + wbtSize r' + wbtSize r
        s1 = 1 + s2 + wbtSize l'

-- | Double left rotation
doubleLR :: WBT k -> WBT k
doubleLR (Nwbt k0 s0 l0 (Nwbt k1 s1 (Nwbt k2 s2 l2 r2) r1)) =
  Nwbt k2 (s0 + s1 + 1) (Nwbt k0 s3 l0 l2) (Nwbt k1 s4 r2 r1)
  where s3 = wbtSize l0 + wbtSize l2 + 1
        s4 = wbtSize r2 + wbtSize r1 + 1

-- | Double right rotation
doubleRR :: WBT k -> WBT k
doubleRR (Nwbt k0 s0 (Nwbt k1 s1 l1 (Nwbt k2 s2 l2 r2)) r0) =
  Nwbt k2 (s1 + s0 + 1) (Nwbt k1 s3 l1 l2) (Nwbt k0 s4 r2 r0)
  where s3 = wbtSize l1 + wbtSize l2 + 1
        s4 = wbtSize r2 + wbtSize r0 + 1

-- | Choose the good Left Rotation
rotateL :: WBT k -> WBT k
rotateL (Nwbt k s l r)
  | needSingleR l r = singleLR $ Nwbt k s l r
  | otherwise = doubleLR $ Nwbt k s l r

-- | Choose the good Right Rotation
rotateR :: WBT k -> WBT k
rotateR (Nwbt k s l r)
  | needSingleR r l = singleRR $ Nwbt k s l r
  | otherwise = doubleRR $ Nwbt k s l r

-- | Rotate a WBT if necessary
rotate :: WBT k -> WBT k
rotate (Nwbt k s l r)
  | wbtIsBalanced $ Nwbt k s l r = Nwbt k s l r
  | wbtSize l < wbtSize r = rotateL $ Nwbt k s l r
  | otherwise = rotateR $ Nwbt k s l r

-- | Add a key.
add :: (Ord k) => k -> WBT k -> WBT k
add x Ewbt = wbtLeaf x
add x (Nwbt k s l r)
  | x < k = rotate $ Nwbt k (s + 1) (add x l) r
  | x > k = rotate $ Nwbt k (s + 1) l (add x r)
  | x == k = Nwbt k s l r

-- | Append a WBT to another WBT
wbtAppend :: (Ord k) => WBT k -> WBT k -> WBT k
wbtAppend Ewbt t = t
wbtAppend t Ewbt = t
wbtAppend t (Nwbt k _ l r) = wbtAppend (wbtAppend (add k t) l) r


-- | Build the WBT from a given F.Foldable
fold2wbt :: (F.Foldable f, Ord k) => f k -> WBT k
fold2wbt f = F.foldMap wbtLeaf f

-- | Build the WBT from a list of keys
list2wbt :: (Ord k) => [k] -> WBT k
list2wbt l = mconcat $ map wbtLeaf l

-- | Build a list from the keys of a WBT
wbt2list :: (Ord k) => WBT k -> [k]
wbt2list t = F.foldMap (\x -> [x]) t


