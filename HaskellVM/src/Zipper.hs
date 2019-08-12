--------------------------------------------------------------------------------
module Zipper where
--------------------------------------------------------------------------------

-- | Defining our own Zipper type for lists
data Zipper a = Zipper {
    getLeft :: [a],  -- | Get list in left of Zipper
    view :: a,       -- | Get element in view of Zipper
    getRight :: [a]  -- | Get list in right of Zipper
}

-- | Define type classes for Zipper here!

instance Foldable Zipper where
    -- | foldr :: (a -> b -> b) -> b -> Zipper a -> b
    foldr f z (Zipper ls x rs) = foldr f (f x (foldr f z rs)) ls

-- | Proof in Proofs folder!
instance Functor Zipper where
    -- | fmap :: (a -> b) -> Zipper a -> Zipper b
    fmap f (Zipper ls x rs) = Zipper (fmap f ls) (f x) (fmap f rs)

-- | Find out if follows applicative laws!
instance Applicative Zipper where
    -- | pure :: a -> Zipper a
    pure x = let xs = repeat x in Zipper xs x xs

    -- | Zipper (a -> b) -> Zipper a -> Zipper b
    (Zipper lfs f rfs) <*> (Zipper ls x rs) = 
        Zipper (zipWith h lfs ls) (f x) (zipWith h rfs rs)
            where h = (\g y -> g y)

-- | Define functions for Zipper here!

-- | Traverse the Zipper left
left :: Zipper a -> Zipper a
left (Zipper ls x (r:rs)) = Zipper (x:ls) r rs

moveLeft = left

-- | Traverse the Zipper left SAFELY
safeLeft :: Zipper a -> Maybe (Zipper a)
safeLeft (Zipper _ _ []) = Nothing
safeLeft z               = Just . left $ z

-- | Traverse the Zipper right
right :: Zipper a -> Zipper a
right (Zipper (l:ls) x rs) = Zipper ls l (x:rs)

moveRight = right

-- | Traverse the Zipper right SAFELY
safeRight :: Zipper a -> Maybe (Zipper a)
safeRight (Zipper [] _ _) = Nothing
safeRight z               = Just . right $ z

-- | Transform a Zipper into a list
toList :: Zipper a -> [a]
toList (Zipper ls x rs) = ls ++ x : rs

-- | Create a Zipper from a list
fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs

-- | Create a Zipper from a list SAFELY
safeFromList :: [a] -> Maybe (Zipper a)
safeFromList [] = Nothing
safeFromList z  = Just . fromList $ z
