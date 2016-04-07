{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Shell where

import Prologue hiding (Getter, Setter)

import Data.Cover
import Data.RTuple (TMap, Assocs, Access, Accessible, access, head', tail2, prepend2)
import Type.Applicative


--------------------
-- === Layers === --
--------------------

-- === Definitions === --

newtype Layer  t l  = Layer (LayerData t l)
type    Layers t ls = Layer t <$> ls

type family LayerType   l
type family LayerData t l
type family LayerInfo t a

class    HasLayer l a where layer :: Lens' a (Layer (LayerInfo (LayerType l) a) l)
instance HasLayer I a where layer = impossible
instance HasLayer l I where layer = impossible


-- === Instances === --

-- Wrappers
makeWrapped ''Layer

-- Basic
deriving instance NFData (LayerData t l) => NFData (Layer t l)
deriving instance Show   (LayerData t l) => Show   (Layer t l)

-- Base
type instance LayerType (Layer t l) = t

-- Casting
instance Castable (Unwrapped (Layer t l)) (Unwrapped (Layer t' l')) => Castable (Layer t l) (Layer t' l') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}



--------------------
-- === Shell === --
--------------------

-- === Definitions === --

newtype Shell   t (ls :: [*]) = Shell (TMap (Assocs ls (Layers t ls)))
type    Shelled t (ls :: [*]) = Cover (Shell t ls)
makeWrapped ''Shell


-- === Utils === --

shellLayer :: forall ls l t. (Access l (Assocs ls (Layers t ls)) ~ Layer t l, Accessible l (Assocs ls (Layers t ls)))
           => Lens' (Shell t ls) (Layer t l)
shellLayer = wrapped' . access (Proxy :: Proxy l) ; {-# INLINE shellLayer #-}


-- === Instances === --

type instance Unlayered (Shelled t (l ': ls) a) = Shelled t ls a
instance      Layered   (Shelled t (l ': ls) a) where
    layered = lens (\(Cover s a) -> Cover (s & wrapped %~ (^. tail2)) a) (\(Cover (Shell tmap) _) (Cover s a) -> Cover (s & wrapped %~ prepend2 (tmap ^. (wrapped' . head'))) a)
    {-# INLINE layered #-}



-- type :|
-- -- class
--
-- type family TermOf a
-- data TermLayerType
-- data TermLayer a
-- type instance LayerInfo TermLayerType a = TermLayer (TermOf a)
--
--
-- data Succs
--
-- type instance LayerType Succs = TermLayerType
-- type instance LayerData (TermLayer term) Succs = Int
