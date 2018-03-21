-- Console.hs ---

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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Module.Console
  (
    ConsoleCommandF (..),
    consoleRead,
    consoleWrite
  )
  where

import           Control.Monad.Identity (Identity)
import           Core.Common            (InjectTypeF, injectFree)
import           Core.Types             (Interpretable (..))

data ConsoleCommandF a n = ReadF (a -> n)
                         | WriteF a n

instance Functor (ConsoleCommandF a) where
  fmap g (ReadF f)    = ReadF (g . f)
  fmap f (WriteF x y) = WriteF x (f y)

instance Interpretable Identity (ConsoleCommandF String) where
  interpretM (ReadF f)    = f "console input"
  interpretM (WriteF _ f) = f

instance Interpretable IO (ConsoleCommandF String) where
  interpretM (ReadF f)    = f =<< getLine
  interpretM (WriteF v f) = putStrLn v >> f

consoleRead :: InjectTypeF ConsoleCommandF String String
consoleRead = injectFree (ReadF id)

consoleWrite :: String -> InjectTypeF ConsoleCommandF String ()
consoleWrite v = injectFree (WriteF v ())
