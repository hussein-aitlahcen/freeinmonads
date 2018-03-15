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

module Program.Types where

import           Control.Monad.Free     (Free)
import           Control.Monad.Identity (Identity)
import           Core.Types

{-
############################################################
                  Program Types
############################################################
-}
type Url = String
type ObjectId = Int
data ApiCommandF a n = GetF Url (a -> n)
                     deriving Functor
data ConsoleCommandF a n = ReadF (a -> n)
                         | WriteF a n
                         deriving Functor
data DbCommandF a n = FetchF ObjectId (a -> n)
                    | SaveF ObjectId a n
                    deriving Functor
type ProgramA a b c  = ApiCommandF a :+: DbCommandF b :+: ConsoleCommandF c
type ProgramB a b c  = DbCommandF a :+: ConsoleCommandF b :+: ApiCommandF c
type ProgramAF a b c = Free (ProgramA a b c)
type ProgramBF a b c = Free (ProgramB a b c)
{-
############################################################
                   Pure Interpreter
############################################################
-}
instance Interpretable Identity (DbCommandF String) where
  interpretM (FetchF i f)  = f (show i)
  interpretM (SaveF _ _ f) = f

instance Interpretable Identity (ApiCommandF String) where
  interpretM (GetF _ f) = f "Hello, World !"

instance Interpretable Identity (ConsoleCommandF String) where
  interpretM (ReadF f)    = f "Console input"
  interpretM (WriteF _ f) = f
{-
############################################################
                  Impure Interpreter
############################################################
-}
instance Interpretable IO (DbCommandF String) where
  interpretM (FetchF i f)  = f (show i)
  interpretM (SaveF i _ f) = putStrLn ("Saving ObjectId: " ++ show i) >> f

instance Interpretable IO (ApiCommandF String) where
  interpretM (GetF _ f) = f =<< getLine

instance Interpretable IO (ConsoleCommandF String) where
  interpretM (ReadF f)    = f =<< getLine
  interpretM (WriteF v f) = putStrLn v >> f

