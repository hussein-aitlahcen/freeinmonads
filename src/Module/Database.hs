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
    ObjectId,
    DbCommandF (..),
    dbFetch,
    dbSave
  )
  where

import           Control.Monad.Identity (Identity)
import           Core.Common            (InjectTypeF, injectFree)
import           Core.Types             (Interpretable (..))

type ObjectId = Int

data DbCommandF a n = FetchF ObjectId (a -> n)
                    | SaveF ObjectId a n

instance Functor (DbCommandF a) where
  fmap g (FetchF i f)  = FetchF i (g <$> f)
  fmap f (SaveF i x y) = SaveF i x (f y)

instance Interpretable Identity (DbCommandF String) where
  interpretM (FetchF i f)  = f (show i)
  interpretM (SaveF _ _ f) = f

instance Interpretable IO (DbCommandF String) where
  interpretM (FetchF i f)  = f (show i)
  interpretM (SaveF i _ f) = putStrLn ("effectful computation: " ++ show i) >> f

dbFetch :: ObjectId -> InjectTypeF DbCommandF String String
dbFetch i = injectFree (FetchF i id)

dbSave :: ObjectId -> String -> InjectTypeF DbCommandF String ()
dbSave i s = injectFree (SaveF i s ())
