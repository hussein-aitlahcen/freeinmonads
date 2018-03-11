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
{-# LANGUAGE TypeOperators         #-}

module Types where

import           Control.Monad.Free

type Url = String
type ObjectId = Int

-- Credits to DataTypes A La Carte http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
-- by Wounder Swierstra
data (:+:) f g e = InL (f e) | InR (g e) deriving Functor

data ApiCommandF a n = GetF Url (a -> n)
                     deriving Functor

data ConsoleCommandF a n = ReadF (a -> n)
                         | WriteF a n
                         deriving Functor

data DbCommandF a n = FetchF ObjectId (a -> n)
                    | SaveF ObjectId a n
                    deriving Functor

type Program a b c = ApiCommandF a :+: ConsoleCommandF b :+: DbCommandF c

type ProgramF a b c = Free (Program a b c)

class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a

instance Functor f => f :<: f where
  inj = id

instance {-# OVERLAPS #-} (Functor f, Functor g) => f :<: (g :+: f) where
  inj = InR

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (g :+: h) where
  inj = InL . inj

