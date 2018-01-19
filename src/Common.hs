-- Common.hs ---

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

{-# LANGUAGE TypeOperators #-}

module Common where

import           Control.Monad.Free
import           Types

liftApi :: ApiCommandF a n -> ProgramF a b c n
liftApi = liftF . InL . InL

liftConsole :: ConsoleCommandF a n -> ProgramF b a c n
liftConsole = liftF . InL . InR

liftDb :: DbCommandF a n -> ProgramF b c a n
liftDb = liftF . InR

programExec :: (Functor f, Functor g, Functor h, Monad m)
            => (f (m a) -> m a)
            -> (g (m a) -> m a)
            -> (h (m a) -> m a)
            -> Free (f :+: g :+: h) a
            -> m a
programExec apiExec consExec dbExec prog = iterM exec prog
  where
    exec (InR dbCmd)         = dbExec dbCmd
    exec (InL (InL apiCmd))  = apiExec apiCmd
    exec (InL (InR consCmd)) = consExec consCmd
