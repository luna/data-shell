{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Shell where

import Prologue hiding (Getter, Setter)

import Data.Construction
import Data.Layer.Cover_OLD
import Type.Bool
import Data.RTuple

import Data.Convert
import Data.Layer
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Prop
import Data.Typeable (Proxy(Proxy))


import Control.Lens (view)


--------------------
-- === Layers === --
--------------------

-- === Definitions === --

type family LayerBase l
type family LayerData l base
newtype     Layer     l base = Layer (LayerData l base)


-- === Utils === --

type family Layers ls base where
            Layers '[]       base = '[]
            Layers (l ': ls) base = Layer l base ': Layers ls base


-- === Instances === --

-- Basic
type instance LayerBase (Layer l base) = base

-- Normal Form
deriving instance NFData (LayerData l base) => NFData (Layer l base)

-- Show
deriving instance Show (Unwrapped (Layer l a)) => Show (Layer l a)

-- Wrappers
makeWrapped ''Layer
type instance Uncovered (Layer l a) = Uncovered (Unlayered (Layer l a))
type instance Unlayered (Layer l a) = Unwrapped (Layer l a)
instance      Layered   (Layer l a)

-- Casting
instance Castable (Unwrapped (Layer l a)) (Unwrapped (Layer l' a')) => Castable (Layer l a) (Layer l' a') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}



--------------------
-- === Shell === ---
--------------------

-- === Definitions === --

--type    Shelled (ls :: [*]) (a :: *) = Cover2 (Shell ls a) a
--newtype Shell   (ls :: [*]) (a :: *) = Shell (TMap (Assocs ls (Layers ls a)))
--makeWrapped ''Shell

--type ls :|  a = Shelled ls a
--type ls :|: a = ls :| a ls


---- === Utils === --

--layer :: forall ls l a. (Access l (Assocs ls (Layers ls a)) ~ Layer l a, Accessible l (Assocs ls (Layers ls a)))
--      => Lens' (Shell ls a) (Layer l a)
--layer = wrapped' . access (Proxy :: Proxy l)


---- === Instances === --

--type instance Unlayered (Shelled (l ': ls) a) = Shelled ls a
--instance      Layered   (Shelled (l ': ls) a) where
--    layered = lens (\(Cover2 s a) -> Cover2 (s & wrapped %~ (^. tail2)) a) (\(Cover2 (Shell tmap) _) (Cover2 s a) -> Cover2 (s & wrapped %~ prepend2 (tmap ^. (wrapped' . head'))) a)
--    --layered = lens (\(Cover2 s a) -> Cover2 (s & wrapped %~ (^. tail2)) a) (\(Cover2 (Shell tmap) _) s -> s & covering2 %~ (wrapped %~ prepend2 (tmap ^. (wrapped' . head'))))

