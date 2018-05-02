-- Dude.hs ---

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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Program.Dude
  (
    dude
  )
  where

import           Control.Monad.Free.Church (F)
import           Core.Types                ((:<:))
import           Module.Api                (ApiCommandF)
import           Module.Console            (ConsoleCommandF)
import           Module.Database           (DbCommandF)
import           Program.Cat               (cat)
import           Program.Dog               (dog)

dude :: (Functor f,
        ConsoleCommandF String        :<: f,
        ApiCommandF     String String :<: f,
        DbCommandF      Int    String :<: f)
    => Int
    -> (Int -> String -> a)
    -> F f a
dude k f = do
  x <- dog k
  y <- cat
  pure (f x y)
