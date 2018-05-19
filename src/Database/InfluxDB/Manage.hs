{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-signatures #-}
#else
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
#endif
module Database.InfluxDB.Manage
  ( -- * Management query interface
    Query
  , manage

  -- * Query parameters
  , QueryParams
  , queryParams
  , server
  , database
  , precision
  , manager
  ) where
import Control.Exception
import Control.Monad

import Control.Lens
import Data.Aeson
import Data.Void
import qualified Data.Aeson.Types as A
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT

import Database.InfluxDB.Types as Types
import Database.InfluxDB.Query hiding (query)
import qualified Database.InfluxDB.Format as F

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Database.InfluxDB.Query
-- >>> import Database.InfluxDB.Format ((%))

-- | Send a database management query to InfluxDB.
--
-- >>> let db = "manage-test"
-- >>> let p = queryParams db
-- >>> manage p $ F.formatQuery ("CREATE DATABASE "%F.database) db
manage :: QueryParams -> Query -> IO ()
manage params q = do
  manager' <- either HC.newManager return $ params^.manager
  response <- HC.httpLbs request manager' `catch` (throwIO . HTTPException)
  let body = HC.responseBody response
  case eitherDecode' body of
    Left message ->
      throwIO $ UnexpectedResponse message request body
    Right val -> case A.parse (parseResults (params^.precision)) val of
      A.Success (_ :: V.Vector Void) -> return ()
      A.Error message -> do
        let status = HC.responseStatus response
        when (HT.statusIsServerError status) $
          throwIO $ ServerError message
        when (HT.statusIsClientError status) $
          throwIO $ ClientError message request
        throwIO $ UnexpectedResponse
          ("BUG: " ++ message ++ " in Database.InfluxDB.Manage.manage")
          request
          (encode val)

  where
    request = HC.setQueryString qs $ manageRequest params
    qs =
      [ ("q", Just $ F.fromQuery q)
      ]

manageRequest :: QueryParams -> HC.Request
manageRequest params = HC.defaultRequest
  { HC.host = TE.encodeUtf8 $ params^.server.host
  , HC.port = fromIntegral $ params^.server.port
  , HC.secure = params^.server.ssl
  , HC.method = "POST"
  , HC.path = "/query"
  }
  where
    Server {..} = params^.server
