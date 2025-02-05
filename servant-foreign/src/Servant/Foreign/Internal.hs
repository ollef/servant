{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generalizes all the data needed to make code generation work with
-- arbitrary programming languages.
module Servant.Foreign.Internal where

import Prelude ()
import Prelude.Compat

import Control.Lens (makeLenses, makePrisms, (%~), (&), (.~), (<>~))
import Data.Data (Data)
import Data.Proxy
import Data.Semigroup (Semigroup)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import GHC.TypeLits
import qualified Network.HTTP.Types as HTTP
import Servant.API
import Servant.API.Modifiers (RequiredArgument)

newtype FunctionName = FunctionName { unFunctionName :: [Text] }
  deriving (Data, Show, Eq, Semigroup, Monoid, Typeable)

makePrisms ''FunctionName

data PathSegment f
  = Static Text
    -- ^ a static path segment. like "/foo"
  | Capture Text f
    -- ^ a capture. like "/:userid"
  deriving (Data, Eq, Show, Typeable)

data QueryArgType
  = Normal
  | Flag
  | List
  deriving (Data, Eq, Show, Typeable)

data Url f = Url
  { _path :: [PathSegment f]
  , _queryString :: [(Text, QueryArgType, f)]
  } deriving (Data, Eq, Show, Typeable)

defUrl :: Url f
defUrl = Url [] []

makeLenses ''Url

data Request arg res = Request
  { _url :: Url arg
  , _method :: HTTP.Method
  , _headers :: [(Text, arg)]
  , _body :: Maybe arg
  , _returnType :: Maybe res
  , _functionName :: FunctionName
  } deriving (Data, Eq, Show, Typeable)

makeLenses ''Request

defReq :: Request arg res
defReq = Request defUrl "GET" [] Nothing Nothing (FunctionName [])

-- | 'HasForeignArgument' and 'HasForeignResult' map Haskell types with types in the target
-- language of your backend. For example, let's say you're
-- implementing a backend to some language __X__, and you want
-- a Text representation of each input/output type mentioned in the API:
--
-- > -- First you need to create a dummy type to parametrize your
-- > -- instances.
-- > data LangX
-- >
-- > -- Otherwise you define instances for the types you need
-- > instance HasForeignArgument LangX contentTypes Text Int where
-- >    argumentFor _ _ _ _ = "intX"
-- >
-- > -- Or for example in case of lists
-- > instance HasForeignArgument LangX contentTypes Text a => HasForeignArgument LangX contentTypes Text [a] where
-- >    argumentFor lang contentTypes argType _ = "listX of " <> argumentFor lang contentTypes argType (Proxy :: Proxy a)
--
-- Finally to generate list of information about all the endpoints for
-- an API you create a function of a form:
--
-- > getEndpoints :: (HasForeign LangX Text Text api, GenerateList Text Text (Foreign Text api))
-- >              => Proxy api -> [Request Text]
-- > getEndpoints api = listFromAPI (Proxy :: Proxy LangX) (Proxy :: Proxy Text) (Proxy :: Proxy Text) api
--
class HasForeignArgument lang (contentTypes :: [*]) foreignRep a where
  argumentFor :: Proxy lang -> Proxy contentTypes -> Proxy foreignRep -> Proxy a -> foreignRep

class HasForeignResult lang (contentTypes :: [*]) foreignRep a where
  resultFor :: Proxy lang -> Proxy contentTypes -> Proxy foreignRep -> Proxy a -> foreignRep

class HasForeign lang farg fres (api :: *) where
  type Foreign farg fres api :: *
  foreignFor :: Proxy lang -> Proxy farg -> Proxy fres -> Proxy api -> Request farg fres -> Foreign farg fres api

instance (HasForeign lang farg fres a, HasForeign lang farg fres b)
  => HasForeign lang farg fres (a :<|> b) where
  type Foreign farg fres (a :<|> b) = Foreign farg fres a :<|> Foreign farg fres b

  foreignFor lang farg fres Proxy req =
         foreignFor lang farg fres (Proxy :: Proxy a) req
    :<|> foreignFor lang farg fres (Proxy :: Proxy b) req

data EmptyForeignAPI = EmptyForeignAPI

instance HasForeign lang farg fres EmptyAPI where
  type Foreign farg fres EmptyAPI = EmptyForeignAPI

  foreignFor Proxy Proxy Proxy Proxy _ = EmptyForeignAPI

instance (KnownSymbol sym, HasForeignArgument lang '[Capture' mods sym t] farg t, HasForeign lang farg fres api)
  => HasForeign lang farg fres (Capture' mods sym t :> api) where
  type Foreign farg fres (Capture' mods sym t :> api) = Foreign farg fres api

  foreignFor lang Proxy Proxy Proxy req =
    foreignFor lang Proxy Proxy (Proxy :: Proxy api) $
      req & url . path <>~ [Capture str foreignArg]
          & functionName . _FunctionName %~ (++ ["by", str])
    where
      str = pack . symbolVal $ (Proxy :: Proxy sym)
      foreignArg = argumentFor lang (Proxy :: Proxy '[Capture' mods sym t]) (Proxy :: Proxy farg) (Proxy :: Proxy t)

instance (KnownSymbol sym, HasForeignArgument lang '[CaptureAll sym t] farg [t], HasForeign lang farg fres sublayout)
  => HasForeign lang farg fres (CaptureAll sym t :> sublayout) where
  type Foreign farg fres (CaptureAll sym t :> sublayout) = Foreign farg fres sublayout

  foreignFor lang Proxy Proxy Proxy req =
    foreignFor lang Proxy Proxy (Proxy :: Proxy sublayout) $
      req & url . path <>~ [Capture str foreignArg]
          & functionName . _FunctionName %~ (++ ["by", str])
    where
      str = pack . symbolVal $ (Proxy :: Proxy sym)
      foreignArg = argumentFor lang (Proxy :: Proxy '[CaptureAll sym t]) (Proxy :: Proxy farg) (Proxy :: Proxy [t])

instance (HasForeignResult lang list fres a, ReflectMethod method)
  => HasForeign lang farg fres (Verb method status list a) where
  type Foreign farg fres (Verb method status list a) = Request farg fres

  foreignFor lang Proxy Proxy Proxy req =
    req & functionName . _FunctionName %~ (methodLC :)
        & method .~ method_
        & returnType .~ Just retType
    where
      retType = resultFor lang (Proxy :: Proxy list) (Proxy :: Proxy fres) (Proxy :: Proxy a)
      method_ = reflectMethod (Proxy :: Proxy method)
      methodLC = toLower $ decodeUtf8 method_

-- | TODO: doesn't taking framing into account.
instance (HasForeignResult lang '[ct] fres a, ReflectMethod method)
  => HasForeign lang farg fres (Stream method status framing ct a) where
  type Foreign farg fres (Stream method status framing ct a) = Request farg fres

  foreignFor lang Proxy Proxy Proxy req =
    req & functionName . _FunctionName %~ (methodLC :)
        & method .~ method_
        & returnType .~ Just retType
    where
      retType = resultFor lang (Proxy :: Proxy '[ct]) (Proxy :: Proxy fres) (Proxy :: Proxy a)
      method_ = reflectMethod (Proxy :: Proxy method)
      methodLC = toLower $ decodeUtf8 method_

instance (KnownSymbol sym, HasForeignArgument lang '[Header' mods sym a] farg (RequiredArgument mods a), HasForeign lang farg fres api)
  => HasForeign lang farg fres (Header' mods sym a :> api) where
  type Foreign farg fres (Header' mods sym a :> api) = Foreign farg fres api

  foreignFor lang Proxy Proxy Proxy req =
    foreignFor lang Proxy Proxy subP $ req & headers <>~ [(hname, foreignArg)]
    where
      hname = pack . symbolVal $ (Proxy :: Proxy sym)
      foreignArg =
        argumentFor lang (Proxy :: Proxy '[Header' mods sym a]) (Proxy :: Proxy farg) (Proxy :: Proxy (RequiredArgument mods a))
      subP  = Proxy :: Proxy api

instance (KnownSymbol sym, HasForeignArgument lang '[QueryParam' mods sym a] farg (RequiredArgument mods a), HasForeign lang farg fres api)
  => HasForeign lang farg fres (QueryParam' mods sym a :> api) where
  type Foreign farg fres (QueryParam' mods sym a :> api) = Foreign farg fres api

  foreignFor lang Proxy Proxy Proxy req =
    foreignFor lang (Proxy :: Proxy farg) Proxy (Proxy :: Proxy api) $
      req & url . queryString <>~ [(str, Normal, foreignArg)]
    where
      str = pack . symbolVal $ (Proxy :: Proxy sym)
      foreignArg =
        argumentFor lang (Proxy :: Proxy '[QueryParam' mods sym a]) (Proxy :: Proxy farg) (Proxy :: Proxy (RequiredArgument mods a))

instance
  (KnownSymbol sym, HasForeignArgument lang '[QueryParams sym a] farg [a], HasForeign lang farg fres api)
  => HasForeign lang farg fres (QueryParams sym a :> api) where
  type Foreign farg fres (QueryParams sym a :> api) = Foreign farg fres api
  foreignFor lang Proxy Proxy Proxy req =
    foreignFor lang (Proxy :: Proxy farg) Proxy (Proxy :: Proxy api) $
      req & url . queryString <>~ [(str, List, foreignArg)]
    where
      str = pack . symbolVal $ (Proxy :: Proxy sym)
      foreignArg =
        argumentFor lang (Proxy :: Proxy '[QueryParams sym a]) (Proxy :: Proxy farg) (Proxy :: Proxy [a])

instance
  (KnownSymbol sym, HasForeignArgument lang '[QueryFlag sym] farg Bool, HasForeign lang farg fres api)
  => HasForeign lang farg fres (QueryFlag sym :> api) where
  type Foreign farg fres (QueryFlag sym :> api) = Foreign farg fres api

  foreignFor lang farg fres Proxy req =
    foreignFor lang farg fres (Proxy :: Proxy api) $
      req & url . queryString <>~ [(str, Flag, foreignArg)]
    where
      str = pack . symbolVal $ (Proxy :: Proxy sym)
      foreignArg =
        argumentFor lang (Proxy :: Proxy '[QueryFlag sym]) farg (Proxy :: Proxy Bool)

instance HasForeign lang farg fres Raw where
  type Foreign farg fres Raw = HTTP.Method -> Request farg fres

  foreignFor _ Proxy Proxy Proxy req method_ =
    req & functionName . _FunctionName %~ ((toLower $ decodeUtf8 method_) :)
        & method .~ method_

instance (HasForeignArgument lang list farg a, HasForeign lang farg fres api)
      => HasForeign lang farg fres (ReqBody' mods list a :> api) where
  type Foreign farg fres (ReqBody' mods list a :> api) = Foreign farg fres api

  foreignFor lang farg fres Proxy req =
    foreignFor lang farg fres (Proxy :: Proxy api) $
      req & body .~ (Just $ argumentFor lang (Proxy :: Proxy list) farg (Proxy :: Proxy a))

instance
    ( HasForeign lang farg fres api
    ) =>  HasForeign lang farg fres (StreamBody' mods framing ctype a :> api)
  where
    type Foreign farg fres (StreamBody' mods framing ctype a :> api) = Foreign farg fres api

    foreignFor _lang Proxy Proxy _req = error "HasForeign @StreamBody"

instance (KnownSymbol path, HasForeign lang farg fres api)
      => HasForeign lang farg fres (path :> api) where
  type Foreign farg fres (path :> api) = Foreign farg fres api

  foreignFor lang farg fres Proxy req =
    foreignFor lang farg fres (Proxy :: Proxy api) $
      req & url . path <>~ [Static str]
          & functionName . _FunctionName %~ (++ [str])
    where
      str = pack . symbolVal $ (Proxy :: Proxy path)

instance HasForeign lang farg fres api
  => HasForeign lang farg fres (RemoteHost :> api) where
  type Foreign farg fres (RemoteHost :> api) = Foreign farg fres api

  foreignFor lang farg fres Proxy req =
    foreignFor lang farg fres (Proxy :: Proxy api) req

instance HasForeign lang farg fres api
  => HasForeign lang farg fres (IsSecure :> api) where
  type Foreign farg fres (IsSecure :> api) = Foreign farg fres api

  foreignFor lang farg fres Proxy req =
    foreignFor lang farg fres (Proxy :: Proxy api) req

instance HasForeign lang farg fres api => HasForeign lang farg fres (Vault :> api) where
  type Foreign farg fres (Vault :> api) = Foreign farg fres api

  foreignFor lang farg fres Proxy req =
    foreignFor lang farg fres (Proxy :: Proxy api) req

instance HasForeign lang farg fres api =>
  HasForeign lang farg fres (WithNamedContext name context api) where

  type Foreign farg fres (WithNamedContext name context api) = Foreign farg fres api

  foreignFor lang farg fres Proxy = foreignFor lang farg fres (Proxy :: Proxy api)

instance HasForeign lang farg fres api
  => HasForeign lang farg fres (HttpVersion :> api) where
  type Foreign farg fres (HttpVersion :> api) = Foreign farg fres api

  foreignFor lang farg fres Proxy req =
    foreignFor lang farg fres (Proxy :: Proxy api) req

instance HasForeign lang farg fres api
  => HasForeign lang farg fres (Summary desc :> api) where
  type Foreign farg fres (Summary desc :> api) = Foreign farg fres api

  foreignFor lang farg fres Proxy req =
    foreignFor lang farg fres (Proxy :: Proxy api) req

instance HasForeign lang farg fres api
  => HasForeign lang farg fres (Description desc :> api) where
  type Foreign farg fres (Description desc :> api) = Foreign farg fres api

  foreignFor lang farg fres Proxy req =
    foreignFor lang farg fres (Proxy :: Proxy api) req

-- | Utility class used by 'listFromAPI' which computes
--   the data needed to generate a function for each endpoint
--   and hands it all back in a list.
class GenerateList farg fres reqs where
  generateList :: reqs -> [Request farg fres]

instance GenerateList farg fres EmptyForeignAPI where
  generateList _ = []

instance GenerateList farg fres (Request farg fres) where
  generateList r = [r]

instance (GenerateList farg fres start, GenerateList farg fres rest)
  => GenerateList farg fres (start :<|> rest) where
  generateList (start :<|> rest) = (generateList start) ++ (generateList rest)

-- | Generate the necessary data for codegen as a list, each 'Req'
--   describing one endpoint from your API type.
listFromAPI
  :: (HasForeign lang farg fres api, GenerateList farg fres (Foreign farg fres api))
  => Proxy lang
  -> Proxy farg
  -> Proxy fres
  -> Proxy api
  -> [Request farg fres]
listFromAPI lang farg fres p = generateList (foreignFor lang farg fres p defReq)
