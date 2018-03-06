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

{-# LANGUAGE TypeOperators    #-}

module Common where

import           Control.Monad.Free
import           Types

injectFree :: (Functor f, f :<: g) => f a -> Free g a
injectFree = hoistFree inj . liftF

programExec
  :: (Functor f, Functor g, Functor h, Monad m)
     => (f (m a) -> m a)
     -> (g (m a) -> m a)
     -> (h (m a) -> m a)
     -> Free ((f :+: g) :+: h) a
     -> m a
programExec fExec gExec hExec = iterM exec
  where
    exec (InR hCmd)       = hExec hCmd
    exec (InL (InR gCmd)) = gExec gCmd
    exec (InL (InL fCmd)) = fExec fCmd
