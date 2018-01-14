-- Program.hs ---

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

module Program where

import           Common
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Identity
import           Types

type ProgramA = ProgramF String Int String

apiGet :: String -> ProgramA String
apiGet s = liftApi (GetF s id)

consoleRead :: ProgramA Int
consoleRead = liftConsole (ReadF id)

consoleWrite :: Int -> ProgramA ()
consoleWrite v = liftConsole (WriteF v ())

dbFetch :: Int -> ProgramA String
dbFetch i = liftDb (FetchF i id)

dbSave :: Int -> String -> ProgramA ()
dbSave i s = liftDb (SaveF i s ())

program :: ProgramA String
program = do
  _ <- consoleWrite 1
  apiValue <- apiGet "http://localhost/users"
  _ <- dbSave 0 apiValue
  _ <- consoleWrite 1
  pure $ apiValue

