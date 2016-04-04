{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Data.Shell_OLD where

import Prologue hiding (Getter, Setter)

import Data.Construction
import Data.Layer_OLD.Cover_OLD
import Type.Bool
import Data.RTuple

import Data.Convert
import Data.Layer_OLD
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Prop
import Data.Typeable (Proxy(Proxy))


import Control.Lens (view)
import Data.Shell (Layer(Layer), Layers)


----------------------------------------------------------------------------
-- OLD IMPLEMENTATION
----------------------------------------------------------------------------


----------------------
-- === Attached === --
----------------------

-- === Definitions === --

data Attached t a = Attached t a deriving (Generic, Show, Eq, Ord, Functor, Traversable, Foldable)


-- === Utils === --

type family AttachAll ls a where
            AttachAll '[]       a = a
            AttachAll (l ': ls) a = Attached l (AttachAll ls a)


-- === Instances === --

-- Normal Form
instance (NFData t, NFData a) => NFData (Attached t a)

-- Wrappers
type instance Uncovered (Attached t a) = Uncovered (Unlayered (Attached t a))
type instance Unlayered (Attached t a) = a
instance      Layered   (Attached t a) where
    layered = lens (\(Attached _ a) -> a) (\(Attached d _) a -> Attached d a) ; {-# INLINE layered #-}

-- Construction
instance (Monad m, Creator    m t) => LayerConstructor m (Attached t a) where constructLayer a = flip Attached a <$> create  ; {-# INLINE constructLayer #-}
instance (Monad m, Destructor m t) => LayerDestructor  m (Attached t a) where destructLayer (Attached t a) = a <$ destruct t ; {-# INLINE destructLayer  #-}

-- Casting
instance (Castable a a', Castable t t') => Castable (Attached t a) (Attached t' a') where
    cast (Attached d a) = Attached (cast d) (cast a) ; {-# INLINE cast #-}


------------------------


-- FIXME[WD]: we should remove the following usage of properties and use the new `layer` function instead
-- Attributes
type instance Prop prop (Attached (Layer l a) base) = If (prop == l) (Unwrapped (Layer l a)) (Prop prop base)

instance {-# OVERLAPPABLE #-} (Prop  a (Attached (Layer a' t) base) ~ Prop a base, Getter a base)
                           => Getter a (Attached (Layer a' t) base) where getter a (Attached _ t) = getter a t ; {-# INLINE getter #-}
instance {-# OVERLAPPABLE #-} Getter a (Attached (Layer a  t) base) where getter _ (Attached d _) = unwrap' d  ; {-# INLINE getter #-}

instance {-# OVERLAPPABLE #-} (Prop  a (Attached (Layer a' t) base) ~ Prop a base, Setter a base)
                           => Setter a (Attached (Layer a' t) base) where setter a v (Attached d t) = Attached d $ setter a v t ; {-# INLINE setter #-}
instance {-# OVERLAPPABLE #-} Setter a (Attached (Layer a  t) base) where setter _ v (Attached _ t) = Attached (Layer v) t      ; {-# INLINE setter #-}


------------------------


-- |:|

newtype (layers :: [*]) :<  (a :: *)        = Shelled_OLD (ShellLayers layers a)
type    (layers :: [*]) :<: (a :: [*] -> *) = layers :< a layers

type ShellLayers ls a = AttachAll (Layers ls a) (Cover a)

--class HasLayer l a where
--    layer :: Lens' a (Layer l (Uncovered a))


--instance {-# OBERLAPPABLE #-} base ~ Uncovered a => HasLayer l (Attached (Layer l base) a) where layer = lens (\(Attached l _) -> l) (\(Attached _ a) l -> Attached l a) ; {-# INLINE layer #-}
--instance {-# OBERLAPPABLE #-} HasLayer l a       => HasLayer l (Attached t              a) where layer = wrapped' ∘ layer                                                ; {-# INLINE layer #-}

-- === Utils === --

type family ReShelled a where ReShelled (t ls) = ls :<: t


-- === Instances === --

-- Basic
deriving instance Show (Unwrapped (ls :< a)) => Show (ls :< a)

-- Normal Form
deriving instance NFData (Unwrapped (ls :< a)) => NFData (ls :< a)

-- Wrappers
makeWrapped ''(:<)
type instance Uncovered (ls :< a) = a
type instance Unlayered (ls :< a) = Unwrapped (ls :< a)
instance      Layered   (ls :< a)

-- Construction
instance Monad m => LayerConstructor m (ls :< a) where constructLayer = return ∘ wrap'   ; {-# INLINE constructLayer #-}
instance Monad m => LayerDestructor  m (ls :< a) where destructLayer  = return ∘ unwrap' ; {-# INLINE destructLayer  #-}

instance (Monad m, CoverConstructor m (ls :< a), Creator m a) => Creator m (ls :< a) where
    create = constructCover =<< create ; {-# INLINE create #-}

-- Conversion
instance {-# OVERLAPPABLE #-}                                                           Castable (ls :< a) (ls  :< a)  where cast = id              ; {-# INLINE cast #-}
instance {-# OVERLAPPABLE #-} Castable (Unwrapped (ls :< a)) (Unwrapped (ls' :< a')) => Castable (ls :< a) (ls' :< a') where cast = wrapped %~ cast ; {-# INLINE cast #-}

-- Attributes
type instance                                Prop a (ls :< t) = Prop a (Unwrapped (ls :< t))
instance Getter a (Unwrapped (ls :< t)) => Getter a (ls :< t) where getter a = getter a ∘ unwrap'      ; {-# INLINE getter #-}
instance Setter a (Unwrapped (ls :< t)) => Setter a (ls :< t) where setter   = over wrapped' ∘∘ setter ; {-# INLINE setter #-}








