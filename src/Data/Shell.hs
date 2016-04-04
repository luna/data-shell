{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Shell where

import Prologue hiding (Getter, Setter)

import Data.Construction
import Data.Cover
import Type.Bool

import           Data.RTuple (TMap, Assocs, Access, Accessible, access, head', tail2, prepend2)
import qualified Data.RTuple as List

import Data.Convert
import Data.Layer_OLD
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

makeWrapped ''Layer


-- === Utils === --

type family Layers ls base where
            Layers '[]       base = '[]
            Layers (l ': ls) base = Layer l base ': Layers ls base


-- === Instances === --

-- Basic
deriving instance NFData (LayerData l a)         => NFData (Layer l a)
deriving instance Show   (Unwrapped (Layer l a)) => Show   (Layer l a)

-- Base
type instance LayerBase (Layer l a) = a

-- Casting
instance Castable (Unwrapped (Layer l a)) (Unwrapped (Layer l' a')) => Castable (Layer l a) (Layer l' a') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}


--------------------
-- === Shell === ---
--------------------

-- === Definitions === --

type    Shelled (ls :: [*]) (a :: *) = Cover (Shell ls a) a
newtype Shell   (ls :: [*]) (a :: *) = Shell (TMap (Assocs ls (Layers ls a)))
makeWrapped ''Shell

type ls :|  a = Shelled ls a
type ls :|: a = ls :| a ls


-- === Utils === --

layer :: forall ls l a. (Access l (Assocs ls (Layers ls a)) ~ Layer l a, Accessible l (Assocs ls (Layers ls a)))
      => Lens' (Shell ls a) (Layer l a)
layer = wrapped' . access (Proxy :: Proxy l)


-- === Instances === --

type instance Unlayered (Shelled (l ': ls) a) = Shelled ls a
instance      Layered   (Shelled (l ': ls) a) where
    layered = lens (\(Cover s a) -> Cover (s & wrapped %~ (^. tail2)) a) (\(Cover (Shell tmap) _) (Cover s a) -> Cover (s & wrapped %~ prepend2 (tmap ^. (wrapped' . head'))) a)

