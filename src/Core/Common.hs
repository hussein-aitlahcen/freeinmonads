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

module Core.Common
  (
    injectFree,
    programExec
  )
  where

import           Control.Monad.Free (Free, hoistFree, liftF, iterM)
import           Core.Types (Interpretable(..), (:<:)(..))

injectFree :: (Functor f, Functor g, f :<: g) => f a -> Free g a
injectFree = hoistFree inj . liftF

programExec :: (Functor f, Interpretable m f, Monad m)
               => Free f a
               -> m a
programExec = iterM interpretM
