-- Database.hs ---

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

module Module.Database
  (
    DbCommandF (..),
    dbFetch,
    dbSave
  )
  where

import           Control.Monad.Identity (Identity)
import           Core.Common            (InjectTypeF, injectF)
import           Core.Types             (Interpretable (..))
import           Data.Semigroup         ((<>))

data DbCommandF i a n = FetchF i (a -> n)
                    | SaveF i a n

instance Functor (DbCommandF i a) where
  fmap g (FetchF x f)  = FetchF x (g . f)
  fmap f (SaveF x y z) = SaveF x y (f z)

instance (Show i) => Interpretable Identity (DbCommandF i String) where
  interpretM (FetchF x f)  = f (show x)
  interpretM (SaveF _ _ f) = f

instance (Show i) => Interpretable IO (DbCommandF i String) where
  interpretM (FetchF x f)  = f (show x)
  interpretM (SaveF x _ f) = putStrLn ("effectful computation: " <> show x) >> f

dbFetch :: i -> InjectTypeF (DbCommandF i a) a
dbFetch x = injectF (FetchF x id)

dbSave :: i -> a -> InjectTypeF (DbCommandF i a) ()
dbSave x y = injectF (SaveF x y ())
