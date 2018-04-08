-- Spec.hs ---

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

import           Control.Monad.Free     (Free)
import           Control.Monad.Identity (runIdentity)
import           Core.Common            (programExec)
import           Core.Types             ((:+:) (..))
import           Data.Semigroup         ((<>))
import           Module.Api
import           Module.Console
import           Module.Database
import           Program.Cat            (cat)
import           Program.Dog            (dog)
import           Program.Dude           (dude)
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "how free am I in this monad ?" $
    it "is obviously freedom" $
      let
        --- Top level explicit types
        outputA = (runIdentity . programExec) (cat        :: Free (ApiCommandF String :+: DbCommandF String :+: ConsoleCommandF String) String)
        outputB = (runIdentity . programExec) (dog 2      :: Free (ConsoleCommandF String :+: DbCommandF String :+: ApiCommandF String) Int)
        outputC = (runIdentity . programExec) (dude 4 (\a b -> b <> show a) :: Free (DbCommandF String :+: ConsoleCommandF String :+: ApiCommandF String) String)
      in
        (outputA, outputB, outputC) `shouldBe` ("Hello, World !", 4, "Hello, World !16")
