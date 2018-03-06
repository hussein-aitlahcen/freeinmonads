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

import           Common
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Identity
import           Program
import           Test.Hspec
import           Types

main :: IO ()
main = hspec $
  describe "how free am I in this monad ?" $
    it "is obviously freedom" $
      let
        --- Pure interpreter can be replaced by the impure one
        output = runIdentity $ programExec pureApiExec pureConsExec pureDbExec program
      in
        output `shouldBe` "Hello, World !"

{-
############################################################
                  Impure Interpreter
############################################################
-}
dbExec :: DbCommandF String (IO String) -> IO String
dbExec (FetchF i f)  = f (show i)
dbExec (SaveF i s f) = putStrLn ("Saving ObjectId: " ++ show i) >> f

apiExec :: ApiCommandF String (IO String) -> IO String
apiExec (GetF s f) = f =<< getLine

consExec :: ConsoleCommandF String (IO String) -> IO String
consExec (ReadF f)    = f =<< getLine
consExec (WriteF v f) = putStrLn v >> f

{-
############################################################
                   Pure Interpreter
############################################################
-}
pureDbExec :: DbCommandF String (Identity String) -> Identity String
pureDbExec (FetchF i f)  = f (show i)
pureDbExec (SaveF i s f) = f

pureApiExec :: ApiCommandF String (Identity String) -> Identity String
pureApiExec (GetF s f) = f "Hello, World !"

pureConsExec :: ConsoleCommandF String (Identity String) -> Identity String
pureConsExec (ReadF f)    = f "Console input"
pureConsExec (WriteF v f) = f
