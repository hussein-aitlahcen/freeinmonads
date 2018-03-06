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

type ProgramA = Free (ProgramF String String String)

apiGet :: Url -> ProgramA String
apiGet s = inFree (GetF s id)

consoleRead :: ProgramA String
consoleRead = inFree (ReadF id)

consoleWrite :: String -> ProgramA ()
consoleWrite v = inFree (WriteF v ())

dbFetch :: ObjectId -> ProgramA  String
dbFetch i = inFree (FetchF i id)

  dbSave :: ObjectId -> String -> ProgramA  ()
dbSave i s = inFree (SaveF i s ())

program :: ProgramA String
program = do
  consoleWrite "befire api get"
  a <- consoleRead
  consoleWrite a
  apiValue <- apiGet "http://localhost/users"
  dbSave 10 apiValue
  consoleWrite "after api get & db save"
  pure apiValue
