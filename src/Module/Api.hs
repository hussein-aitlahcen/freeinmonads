-- Api.hs ---

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

module Module.Api
  (
    ApiCommandF (..),
    apiGet
  )
  where

import           Control.Monad.Identity (Identity)
import           Core.Common            (InjectTypeF, injectF)
import           Core.Types             (Interpretable (..))

data ApiCommandF u a n = GetF u (a -> n)

instance Functor (ApiCommandF u a) where
  fmap g (GetF url f) = GetF url (g . f)

instance Interpretable Identity (ApiCommandF u String) where
  interpretM (GetF _ f) = f "Hello, World !"

instance Interpretable IO (ApiCommandF u String) where
  interpretM (GetF _ f) = f =<< getLine

apiGet :: u -> InjectTypeF (ApiCommandF u a) a
apiGet url = injectF (GetF url id)
