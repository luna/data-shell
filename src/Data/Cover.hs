module Data.Cover where

import Prologue
import Control.Lens.Property


-- === Definitions ===

data Covered  = Covered  deriving (Show)
data Covering = Covering deriving (Show)

data Cover cover a = Cover !cover !a deriving (Generic, Show, Eq, Ord, Functor, Traversable, Foldable)


-- === Utils ===

-- type Uncovered a = View Covered  a
-- type Hollowed  a = View Covering a

-- covered  :: HasProperty Covered a => Property  Covered a
-- covered' :: HasProperty Covered a => Property' Covered a
-- covered  = prop  Covered ; {-# INLINE covered  #-}
-- covered' = prop' Covered ; {-# INLINE covered' #-}
--
-- covering  :: HasProperty Covering a => Property  Covering a
-- covering' :: HasProperty Covering a => Property' Covering a
-- covering  = prop  Covering ; {-# INLINE covering  #-}
-- covering' = prop' Covering ; {-# INLINE covering' #-}
--
-- uncover :: HasProperty Covered a => a -> Uncovered a
-- uncover = view covered ; {-# INLINE uncover #-}
--
-- hollow :: HasProperty Covering a => a -> Hollowed a
-- hollow = view covering ; {-# INLINE hollow #-}

--
-- -- === Instances === --
--
-- -- Covered
-- type instance View Covered   (Cover c a) = a
-- type instance Set  Covered v (Cover c a) = Cover c v
-- instance HasProperty Covered (Cover c a) where
--     property _ = lens (\(Cover _ a) -> a) (\(Cover c _) a -> Cover c a)
--     {-# INLINE property #-}
--
-- -- Covering
-- type instance View Covering   (Cover c a) = c
-- type instance Set  Covering v (Cover c a) = Cover v a
-- instance HasProperty Covering (Cover c a) where
--     property _ = lens (\(Cover c _) -> c) (\(Cover _ a) c -> Cover c a)
--     {-# INLINE property #-}
