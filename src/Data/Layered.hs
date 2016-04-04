module Data.Layered where

import Prelude
import Control.Lens.Property
import Control.Lens


-- === Definitions === --

data Layered = Layered deriving (Show)


-- === Utils === --

type Unlayered a = View Layered a

layered  :: HasProperty Layered a => Property  Layered a
layered' :: HasProperty Layered a => Property' Layered a
layered  = prop  Layered ; {-# INLINE layered  #-}
layered' = prop' Layered ; {-# INLINE layered' #-}

unlayer :: HasProperty Layered a => a -> Unlayered a
unlayer = view layered ; {-# INLINE unlayer #-}
