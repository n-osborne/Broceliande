module BST
  ()
  where

import Data.Maybe
import Data.Monoid
import qualified Data.Foldable as F


data BST k = EmptyBST | NodeBST (BST k) k (BST k) deriving (Show)

-- | BST is an instance of Functor
instance Functor BST where
  fmap f EmptyBST = EmptyBST
  fmap f (NodeBST l k r) = NodeBST (fmap f l) (f k) (fmap f r)

-- | BST is an instance of Monoid
instance (Ord k) => Monoid (BST k) where
  mempty = EmptyBST
  mappend = bstAppend

-- | BST is an instance of F.Foldable
instance F.Foldable BST where
  foldMap f EmptyBST = mempty
  foldMap f (NodeBST l k r) = F.foldMap f l `mappend`
                                f k `mappend`
                                F.foldMap f r

-- Some primitives

-- | Construct a leaf with the given key
bstLeaf :: k -> BST k
bstLeaf key = NodeBST EmptyBST key EmptyBST

-- | Emptiness predicate.
bstIsEmpty :: (BST k) -> Bool
bstIsEmpty EmptyBST = True
bstIsEmpty (NodeBST _ _ _) = False

-- | Leaf Predicate.
bstIsLeaf :: (BST k) -> Bool
bstIsLeaf EmptyBST = False
bstIsLeaf (NodeBST l k r)
  | (bstIsEmpty l) && (bstIsEmpty r) = True
  | otherwise = False

-- | elem predicate
bstElem :: (Ord k) => k -> BST k -> Bool
bstElem _ EmptyBST = False
bstElem x (NodeBST l k r)
  | x == k = True
  | x < k = bstElem x l
  | x > k = bstElem x r
  
-- | Get the key in a Maybe
bstKey :: (BST k) -> Maybe k
bstKey EmptyBST = Nothing
bstKey (NodeBST l k r) = Just k

-- | Get left node in Maybe
bstLeft :: (BST k) -> Maybe (BST k)
bstLeft EmptyBST = Nothing
bstLeft (NodeBST l k r) = Just l

-- | Get right node in Maybe
bstRight :: (BST k) -> Maybe (BST k)
bstRight EmptyBST = Nothing
bstRight (NodeBST l k r) = Just r 

-- | Append a BST to another BST
bstAppend :: (Ord k) => (BST k) -> (BST k) -> (BST k)
bstAppend EmptyBST bst = bst
bstAppend bst EmptyBST = bst
bstAppend bst (NodeBST l k r) = bstAppend (bstAppend (bstAdd k bst) l) r

-- | Add a new key to a BST
bstAdd :: (Ord k) => k -> (BST k) -> (BST k)
bstAdd k EmptyBST = bstLeaf k
bstAdd k (NodeBST l' k' r')
  | k < k' = NodeBST (bstAdd k l') k' r'
  | k > k' = NodeBST l' k' (bstAdd k r')
  | k == k' = (NodeBST l' k' r')

-- | Build the list of keys from a BST
bst2list :: (Ord k) => (BST k) -> [k]
bst2list t = F.foldMap (\x -> [x]) t

-- | Build a BST from a list of keys
list2bst :: (Ord k) => [k] -> BST k
list2bst list = mconcat $ map bstLeaf list

-- | Build the bst from a given F.Foldable
fold2bst :: (F.Foldable f, Ord k) => f k -> BST k
fold2bst f = F.foldMap bstLeaf f

