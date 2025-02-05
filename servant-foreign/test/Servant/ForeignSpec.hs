{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.ForeignSpec where

import           Data.Monoid
                 ((<>))
import           Data.Proxy
import           Servant.Test.ComprehensiveAPI
import           Servant.Foreign
import           Servant.Types.SourceT
                 (SourceT)

import           Test.Hspec


spec :: Spec
spec = describe "Servant.Foreign" $ do
  camelCaseSpec
  listFromAPISpec

camelCaseSpec :: Spec
camelCaseSpec = describe "camelCase" $ do
  it "converts FunctionNames to camelCase" $ do
    camelCase (FunctionName ["post", "counter", "inc"])
      `shouldBe` "postCounterInc"
    camelCase (FunctionName ["get", "hyphen-ated", "counter"])
      `shouldBe` "getHyphen-atedCounter"

----------------------------------------------------------------------

-- This declaration simply checks that all instances are in place.
_ = listFromAPI (Proxy :: Proxy LangX) (Proxy :: Proxy String) (Proxy :: Proxy String) comprehensiveAPIWithoutRaw

----------------------------------------------------------------------

data LangX

instance HasForeignResult LangX any String NoContent where
  resultFor _ _ _ _ = "voidX"

instance HasForeignResult LangX any String (Headers ctyps NoContent) where
  resultFor _ _ _ _ = "voidX"

instance HasForeignArgument LangX any String Int where
  argumentFor _ _ _ _ = "arg intX"

instance HasForeignResult LangX any String Int where
  resultFor _ _ _ _ = "res intX"

instance HasForeignArgument LangX any String (SourceT m a) where
  argumentFor _ _ _ _ = "arg streamTX"

instance HasForeignResult LangX any String (SourceT m a) where
  resultFor _ _ _ _ = "res streamTX"

instance HasForeignArgument LangX any String Bool where
  argumentFor _ _ _ _ = "boolX"

instance {-# OVERLAPPING #-} HasForeignArgument LangX any String String where
  argumentFor _ _ _ _ = "stringX"

instance {-# OVERLAPPABLE #-} HasForeignArgument LangX any String a => HasForeignArgument LangX any String [a] where
  argumentFor lang cs farg _ = "arg listX of " <> argumentFor lang cs farg (Proxy :: Proxy a)

instance {-# OVERLAPPABLE #-} HasForeignResult LangX any String a => HasForeignResult LangX any String [a] where
  resultFor lang cs fres _ = "res listX of " <> resultFor lang cs fres (Proxy :: Proxy a)

instance (HasForeignArgument LangX any String a) => HasForeignArgument LangX any String (Maybe a) where
  argumentFor lang cs farg _ = "maybe " <> argumentFor lang cs farg (Proxy :: Proxy a)

type TestApi
    = "test" :> Header "header" [String] :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "test" :> QueryParam "param" Int :> ReqBody '[JSON] [String] :> Post '[JSON] NoContent
 :<|> "test" :> QueryParams "params" Int :> ReqBody '[JSON] String :> Put '[JSON] NoContent
 :<|> "test" :> Capture "id" Int :> Delete '[JSON] NoContent
 :<|> "test" :> CaptureAll "ids" Int :> Get '[JSON] [Int]
 :<|> "test" :> EmptyAPI

testApi :: [Request String String]
testApi = listFromAPI (Proxy :: Proxy LangX) (Proxy :: Proxy String) (Proxy :: Proxy String) (Proxy :: Proxy TestApi)

listFromAPISpec :: Spec
listFromAPISpec = describe "listFromAPI" $ do
  it "generates 5 endpoints for TestApi" $ do
    length testApi `shouldBe` 5

  let [getReq, postReq, putReq, deleteReq, captureAllReq] = testApi

  it "collects all info for get request" $ do
    shouldBe getReq $ defReq
      { _url = Url
          [Static "test"]
          [("flag", Flag, "boolX")]
      , _method = "GET"
      , _headers = [("header", "maybe arg listX of stringX")]
      , _body = Nothing
      , _returnType = Just "res intX"
      , _functionName = FunctionName ["get", "test"]
      }

  it "collects all info for post request" $ do
    shouldBe postReq $ defReq
      { _url = Url
          [Static "test"]
          [("param", Normal, "maybe arg intX")]
      , _method = "POST"
      , _headers = []
      , _body = Just "arg listX of stringX"
      , _returnType = Just "voidX"
      , _functionName = FunctionName ["post", "test"]
      }

  it "collects all info for put request" $ do
    shouldBe putReq $ defReq
      { _url = Url
          [Static "test"]
          -- Shoud this be |intX| or |listX of intX| ?
          [("params", List, "arg listX of arg intX")]
      , _method = "PUT"
      , _headers = []
      , _body = Just "stringX"
      , _returnType = Just "voidX"
      , _functionName = FunctionName ["put", "test"]
      }

  it "collects all info for delete request" $ do
    shouldBe deleteReq $ defReq
      { _url = Url
          [ Static "test"
          , Capture "id" "arg intX"
          ]
          []
      , _method = "DELETE"
      , _headers = []
      , _body = Nothing
      , _returnType = Just "voidX"
      , _functionName = FunctionName ["delete", "test", "by", "id"]
      }

  it "collects all info for capture all request" $ do
    shouldBe captureAllReq $ defReq
      { _url = Url
          [ Static "test"
          , Capture "ids" "arg listX of arg intX" ]
          []
      , _method = "GET"
      , _headers = []
      , _body = Nothing
      , _returnType = Just "res listX of res intX"
      , _functionName = FunctionName ["get", "test", "by", "ids"]
      }
