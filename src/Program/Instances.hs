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

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}

module Program.Instances where

import           Core.Common
import           Core.Types
import           Program.Types

import           Control.Monad.Free

type InjectableF f a b = forall g. (Functor g, f a :<: g) => Free g b

apiGet :: Url -> InjectableF ApiCommandF String String
apiGet s = injectFree (GetF s id)

consoleRead :: InjectableF ConsoleCommandF String String
consoleRead = injectFree (ReadF id)

consoleWrite :: String -> InjectableF ConsoleCommandF String ()
consoleWrite v = injectFree (WriteF v ())

dbFetch :: ObjectId -> InjectableF DbCommandF String String
dbFetch i = injectFree (FetchF i id)

dbSave :: ObjectId -> String -> InjectableF DbCommandF String ()
dbSave i s = injectFree (SaveF i s ())

programA :: Free (ApiCommandF String :+: DbCommandF String :+: ConsoleCommandF String) String
programA = do
  consoleWrite "befire api get"
  a <- consoleRead
  consoleWrite a
  apiValue <- apiGet "http://localhost/users"
  dbSave 10 apiValue
  consoleWrite "after api get & db save"
  pure apiValue

programB :: Free (DbCommandF String :+: ConsoleCommandF String :+: ApiCommandF String) String
programB = do
  consoleWrite "befire api get"
  a <- consoleRead
  consoleWrite a
  apiValue <- apiGet "http://localhost/users"
  dbSave 10 apiValue
  consoleWrite "after api get & db save"
  pure apiValue
