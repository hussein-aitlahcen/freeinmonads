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

{-# LANGUAGE TypeOperators #-}

module Program.Dog
  (
    dog
  )
  where

import           Control.Monad.Free (Free)
import           Core.Types         ((:+:))
import           Module.Api         (ApiCommandF, apiGet)
import           Module.Console     (ConsoleCommandF, consoleRead, consoleWrite)
import           Module.Database    (DbCommandF, dbFetch, dbSave)

dog :: Free (ConsoleCommandF String :+: DbCommandF String :+: ApiCommandF String) Int
dog = do
  consoleWrite "programB"
  consoleWrite =<< consoleRead
  _ <- apiGet "http://localhost/users"
  dbSave 10 =<< dbFetch 10
  consoleWrite "end programB"
  pure 4
