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

{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Core.Common
  (
    InjectTypeF,
    injectF,
    execF
  )
  where

import           Control.Monad.Free.Church (F (..), iterM, liftF)
import           Core.Types                ((:<:) (..), Interpretable (..))

type InjectTypeF f n = forall g. (Functor f, Functor g, f :<: g) => F g n

injectF :: (f :<: g) => f a -> F g a
injectF = liftF . inj

execF :: (Monad m, Functor f, Interpretable m f) => F f a -> m a
execF = iterM interpretM
