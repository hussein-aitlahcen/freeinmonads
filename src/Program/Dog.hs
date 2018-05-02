-- Dog.hs ---

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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Program.Dog
  (
    dog
  )
  where

import           Control.Monad.Free.Church (F)
import           Core.Types                ((:<:))
import           Module.Api                (ApiCommandF, apiGet)
import           Module.Console            (ConsoleCommandF, consoleRead,
                                            consoleWrite)
import           Module.Database           (DbCommandF, dbFetch, dbSave)

dog :: (Functor f,
        ConsoleCommandF String        :<: f,
        ApiCommandF     String String :<: f,
        DbCommandF      Int    String :<: f)
    => Int
    -> F f Int
dog x = do
  consoleWrite "programB"
  input :: String <- consoleRead
  consoleWrite input
  _ :: String <- apiGet "http://localhost/users"
  dbObject :: String <- dbFetch (10 :: Int)
  dbSave (10 :: Int) dbObject
  consoleWrite "end programB"
  pure (x * x)
