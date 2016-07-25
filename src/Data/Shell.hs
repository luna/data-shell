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
-- === Stack === --
--------------------

-- === Definitions === --

newtype Stack   ks ls = Stack (TMap (ks :->> Layers ls))
type    Shelled ls = Cover (Stack ls ls)

type ls :| a = Shelled ls a


-- === Instances === --

-- Show
deriving instance Show (Unwrapped (Stack ks ls)) => Show (Stack ks ls)

-- Wrappers
makeWrapped ''Stack
type instance Unlayered (Shelled (l ': ls) a) = Shelled ls a
instance      Layered   (Shelled (l ': ls) a) where
    layered = lens (\                       (Cover c a) -> Cover (c & wrapped %~ (^. tail2)) a)
                   (\(Cover (Stack tmap) _) (Cover c a) -> Cover (c & wrapped %~ prepend2 (tmap ^. (wrapped' . head'))) a)
    {-# INLINE layered #-}

-- HasLayer
type     ShellLayer l ks ls = (Accessible l (Assocs ks (FMap Layer ls)), RT.Access l (Assocs ks (FMap Layer ls)) ~ Layer l)
instance ShellLayer l ks ls            => HasLayer' l (Stack   ks ls) where layer' = wrapped' . access (Proxy :: Proxy l) ; {-# INLINE layer' #-}
instance HasLayer'  l (Stack ls ls) => HasLayer' l (Shelled ls a) where layer' = covering' ∘ layer'                   ; {-# INLINE layer' #-}

-- Creator
instance (Monad m, Creator m (Unwrapped (Stack ks ls))) => Creator m (Stack ks ls) where
    create = wrap' <$> create ; {-# INLINE create #-}
