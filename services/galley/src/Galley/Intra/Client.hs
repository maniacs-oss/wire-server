{-# LANGUAGE OverloadedStrings #-}

module Galley.Intra.Client
    ( lookupClients
    ) where

import Bilge hiding (options, getHeader, statusCode)
import Bilge.RPC
import Bilge.Retry
import Galley.App
import Galley.Options
import Galley.Types (UserClients)
import Control.Lens (view)
import Control.Retry
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (intercalate)
import Data.ByteString.Conversion
import Data.Id
import Data.Misc (portNumber)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Util.Options

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT

lookupClients :: [UserId] -> Galley UserClients
lookupClients uids = do
    (h, p) <- brigReq
    r <- call "brig"
        $ method GET . host h . port p
        . path "/i/clients"
        . queryItem "ids" users
        . expect2xx
    parseResponse (Error status502 "server-error") r
  where
    users   = intercalate "," $ toByteString' <$> uids


-----------------------------------------------------------------------------
-- Helpers
brigReq :: Galley (ByteString, Word16)
brigReq = do
    h <- encodeUtf8 <$> view (options.optBrig.epHost)
    p <- portNumber . fromIntegral <$> view (options.optBrig.epPort)
    return (h, p)

call :: LT.Text -> (Request -> Request) -> Galley (Response (Maybe LB.ByteString))
call n r = recovering x1 rpcHandlers (const (rpc n r))

x1 :: RetryPolicy
x1 = limitRetries 1
