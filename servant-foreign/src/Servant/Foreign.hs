-- | Generalizes all the data needed to make code generation work with
-- arbitrary programming languages.
module Servant.Foreign
  ( ArgType(..)
  , PathSegment(..)
  , QueryArg(..)
  , Req(..)
  , ReqBodyContentType(..)
  , Url(..)
    -- aliases
  , Path
  , Arg(..)
  , FunctionName(..)
    -- lenses
  , argName
  , argType
  , reqUrl
  , reqMethod
  , reqHeaders
  , reqBody
  , reqBodyContentType
  , reqReturnType
  , reqFuncName
  , path
  , queryStr
  , queryArgName
  , queryArgType
    -- prisms
  , _Normal
  , _Flag
  , _List
    -- rest of it
  , HasForeign(..)
  , HasForeignArgument(..)
  , HasForeignResult(..)
  , GenerateList(..)
  , defReq
  , listFromAPI
    -- re-exports
  , module Servant.API
  , module Servant.Foreign.Inflections
  ) where

import           Servant.API
import           Servant.Foreign.Inflections
import           Servant.Foreign.Internal
