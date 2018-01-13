-- Free.hs ---

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
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module Free where

import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Control.Monad.IO.Class

data CommandF a n where
  ReadF        :: (a -> n) -> CommandF a n
  WriteF       :: a -> n  -> CommandF a n
  WithCommandF :: Free (CommandF a) a -> (a -> n) -> CommandF a n

instance Functor (CommandF a) where
  fmap f (ReadF nf)         = ReadF (f . nf)
  fmap f (WriteF s nf)      = WriteF s (f nf)
  fmap f (WithCommandF b g) = WithCommandF b (f . g)

makeFreeCon 'ReadF
makeFreeCon 'WriteF
makeFreeCon_ 'WithCommandF
withCommandF :: MonadFree (CommandF a) m => Free (CommandF a) a -> m a

runCommand :: Monad m => m a -> (a -> m ()) -> Free (CommandF a) a -> m a
runCommand inf outf = iterM exec
  where
    exec (ReadF f)          = inf >>= f
    exec (WriteF v f)       = outf v >> f
    exec (WithCommandF b f) = runCommand inf outf b >>= f

