-- Types.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Core.Types
  (
    Interpretable (..),
    (:+:) (..),
    (:<:) (..)
  )
  where

{-
####################################################################

                          Abstract types
          Credits to DataTypes A La Carte by Wounder Swierstra
http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf

####################################################################
-}
data (:+:) f g e = InL (f e) | InR (g e) deriving Functor

class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a
  proj :: g a -> Maybe (f a)

instance Functor f => f :<: f where
  inj = id
  proj = Just

instance {-# OVERLAPS #-} (Functor f, Functor g) => g :<: (f :+: g) where
  inj = InR
  proj (InR fa) = Just fa
  proj _        = Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (g :+: h) where
  inj = InL <$> inj
  proj (InL gfa) = proj gfa
  proj _         = Nothing

class (Monad m, Functor f) => Interpretable m f where
  interpretM :: f (m a) -> m a

instance (Monad m, Functor f, Functor g, Interpretable m f, Interpretable m g) => Interpretable m (f :+: g) where
  interpretM (InR ga) = interpretM ga
  interpretM (InL fa) = interpretM fa

