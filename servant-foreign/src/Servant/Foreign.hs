-- | Generalizes all the data needed to make code generation work with
-- arbitrary programming languages.
module Servant.Foreign
  ( module Servant.Foreign.Internal
  , module Servant.Foreign.Inflections
  , module Servant.API
  ) where

import Servant.API
import Servant.Foreign.Inflections
import Servant.Foreign.Internal
