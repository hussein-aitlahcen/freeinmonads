-- Instances.hs ---

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
{-# LANGUAGE RankNTypes       #-}

module Program.Instances where

import           Core.Common
import           Core.Types
import           Program.Types

apiGet :: Url -> InjectTypeF ApiCommandF String String
apiGet s = injectFree (GetF s id)

consoleRead :: InjectTypeF ConsoleCommandF String String
consoleRead = injectFree (ReadF id)

consoleWrite :: String -> InjectTypeF ConsoleCommandF String ()
consoleWrite v = injectFree (WriteF v ())

dbFetch :: ObjectId -> InjectTypeF DbCommandF String String
dbFetch i = injectFree (FetchF i id)

dbSave :: ObjectId -> String -> InjectTypeF DbCommandF String ()
dbSave i s = injectFree (SaveF i s ())

programA :: ProgramAF String String String String
programA = do
  consoleWrite "befire api get"
  a <- consoleRead
  consoleWrite a
  apiValue <- apiGet "http://localhost/users"
  dbSave 10 apiValue
  consoleWrite "after api get & db save"
  pure apiValue

programB :: ProgramBF String String String Int
programB = do
  consoleWrite "befire api get"
  a <- consoleRead
  consoleWrite a
  apiValue <- apiGet "http://localhost/users"
  dbSave 10 apiValue
  consoleWrite "after api get & db save"
  pure 4
