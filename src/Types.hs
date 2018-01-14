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

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

module Types where

import Control.Monad.Free

type Url = String

data (f :+: g) e = InL (f e) | InR (g e) deriving Functor

data ApiCommandF a n = GetF Url (a -> n)
                     deriving Functor

data ConsoleCommandF a n = ReadF (a -> n)
                         | WriteF a n
                         deriving Functor

data DbCommandF a n = FetchF Int (a -> n)
                    | SaveF Int a n
                    deriving Functor

type ProgramF a b c = Free (ApiCommandF a :+: ConsoleCommandF b :+: DbCommandF c)
