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

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Identity
import           Free
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "dsl is working dude" $ do
    it "should sum two inputs" $ do
      let (Identity x) = testRun 5
      x `shouldBe` (5 + 5)

-- Hand made monad
test :: Free (CommandF Int) Int
test = do
  n <- withCommandF $ do
    x <- readF
    y <- readF
    pure $ x + y
  pure n

-- testRun :: IO Int
-- testRun = runCommand (read <$> getLine) (putStrLn . show) test

testRun :: Int -> Identity Int
testRun input = runCommand (pure input) (const . pure $ ()) test

