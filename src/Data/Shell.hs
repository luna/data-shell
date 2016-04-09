{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MagicHash            #-}

module Data.Shell where

import Prologue hiding (Getter, Setter)

import Control.Lens.Property (Set)
import Data.Proxify
import Data.Cover
import Data.RTuple (TMap, Assocs, Accessible, access, head', tail2, prepend2, type (:->>))
import qualified Data.RTuple as RT
import Type.Applicative
import Data.Construction


--------------------
-- === Layers === --
--------------------

-- === Definitions === --

type family LayerData l
newtype     Layer     l  = Layer (LayerData l)
type        Layers    ls = Layer <$> ls


-- === Utils === --

type LayerLens  l a = forall l'. Lens  a (Set (Layer l) (Layer l') a) (Layer l) (Layer l')
type LayerLens' l a =            Lens' a                              (Layer l)

class HasLayer l a where
    layer :: LayerLens l a

class HasLayer' l a where
    layer' :: LayerLens' l a

    default layer' :: (HasLayer l a, Set (Layer l) (Layer l) a ~ a) => Lens' a (Layer l)
    layer' = layer ; {-# INLINE layer' #-}

-- focus

focusLayerProxy  :: HasLayer  l a => Proxy l -> LayerLens  l a
focusLayerProxy' :: HasLayer' l a => Proxy l -> LayerLens' l a
focusLayerProxy  _ = layer  ; {-# INLINE focusLayerProxy  #-}
focusLayerProxy' _ = layer' ; {-# INLINE focusLayerProxy' #-}

focusLayer  :: (HasLayer  l a, l ~ Proxified t) => t -> LayerLens  l a
focusLayer' :: (HasLayer' l a, l ~ Proxified t) => t -> LayerLens' l a
focusLayer  = focusLayerProxy  ∘ proxify ; {-# INLINE focusLayer  #-}
focusLayer' = focusLayerProxy' ∘ proxify ; {-# INLINE focusLayer' #-}

-- access

type family Access t a

accessLayerProxy  :: forall t l a. (HasLayer  l a, l ~ Access t a) => Proxy t -> LayerLens  l a
accessLayerProxy' :: forall t l a. (HasLayer' l a, l ~ Access t a) => Proxy t -> LayerLens' l a
accessLayerProxy  _ = focusLayer  (Proxy :: Proxy l) ; {-# INLINE accessLayerProxy  #-}
accessLayerProxy' _ = focusLayer' (Proxy :: Proxy l) ; {-# INLINE accessLayerProxy' #-}

accessLayer  :: forall t l a. (HasLayer  l a, l ~ Access (Proxified t) a) => t -> LayerLens  l a
accessLayer' :: forall t l a. (HasLayer' l a, l ~ Access (Proxified t) a) => t -> LayerLens' l a
accessLayer  _ = focusLayer  (Proxy :: Proxy l) ; {-# INLINE accessLayer  #-}
accessLayer' _ = focusLayer' (Proxy :: Proxy l) ; {-# INLINE accessLayer' #-}

access' :: (HasLayer' l a, l ~ Access (Proxified t) a) => t -> Lens' a (LayerData l)
access' t = accessLayer' t ∘ wrapped'


-- === Instances === --

-- Show
deriving instance Show (Unwrapped (Layer l)) => Show (Layer l)

-- Wrappers
makeWrapped ''Layer


--------------------
-- === Shell === --
--------------------

-- === Definitions === --

newtype Shell   ls = Shell (TMap (ls :->> Layers ls))
type    Shelled ls = Cover (Shell ls)

type ls :| a = Shelled ls a


-- === Instances === --

-- Show
deriving instance Show (Unwrapped (Shell ls)) => Show (Shell ls)

-- Wrappers
makeWrapped ''Shell
type instance Unlayered (Shelled (l ': ls) a) = Shelled ls a
instance      Layered   (Shelled (l ': ls) a) where
    layered = lens (\                       (Cover c a) -> Cover (c & wrapped %~ (^. tail2)) a)
                   (\(Cover (Shell tmap) _) (Cover c a) -> Cover (c & wrapped %~ prepend2 (tmap ^. (wrapped' . head'))) a)
    {-# INLINE layered #-}

-- HasLayer
type     ShellLayer l ls = (Accessible l (Assocs ls (FMap Layer ls)), RT.Access l (Assocs ls (FMap Layer ls)) ~ Layer l)
instance ShellLayer l ls         => HasLayer' l (Shell   ls  ) where layer' = wrapped' . access (Proxy :: Proxy l) ; {-# INLINE layer' #-}
instance HasLayer'  l (Shell ls) => HasLayer' l (Shelled ls a) where layer' = covering' ∘ layer'                   ; {-# INLINE layer' #-}

-- Creator
instance Creator m (Unwrapped (Shell ls)) => Creator m (Shell ls) where
    create = wrap' <$> create ; {-# INLINE create #-}
