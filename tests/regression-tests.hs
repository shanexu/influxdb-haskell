{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where
import Control.Exception

import Control.Lens
import Data.Time
import Test.Tasty.TH
import Test.Tasty.HUnit
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import Database.InfluxDB
import qualified Database.InfluxDB.Format as F

main :: IO ()
main = $defaultMainGenerator

-- https://github.com/maoe/influxdb-haskell/issues/64
case_issue64 :: Assertion
case_issue64 = withDatabase dbName $ do
  write wp $ Line "count" Map.empty
    (Map.fromList [("value", FieldInt 1)])
    (Nothing :: Maybe UTCTime)
  r <- try $ query qp "SELECT value FROM count"
  case r of
    Left err -> case err of
      UnexpectedResponse message _ _ ->
        message @?=
          "BUG: expected Int, encountered String in Database.InfluxDB.Query.query"
      _ ->
        assertFailure $ got ++ show err
    Right (v :: (V.Vector (Tagged "time" Int, Tagged "value" Int))) ->
      -- NOTE: The time columns should be UTCTime, Text, or String
      assertFailure $ got ++ "no errors: " ++ show v
  where
    dbName = "case_issue64"
    qp = queryParams dbName & precision .~ RFC3339
    wp = writeParams dbName
    got = "expeted an UnexpectedResponse but got "

-- https://github.com/maoe/influxdb-haskell/issues/66
case_issue66 :: Assertion
case_issue66 = do
  r <- try $ query (queryParams "_internal") "SELECT time FROM dummy"
  case r of
    Left err -> case err of
      UnexpectedResponse message _ _ ->
        message @?=
          "BUG: at least 1 non-time field must be queried in Database.InfluxDB.Query.query"
      _ ->
        assertFailure $ got ++ show err
    Right (v :: V.Vector (Tagged "time" Int)) ->
      assertFailure $ got ++ "no errors: " ++ show v
  where
    got = "expected an UnexpectedResponse but got "


withDatabase :: Database -> IO a -> IO a
withDatabase dbName f = bracket_
  (manage q (formatQuery ("CREATE DATABASE "%F.database) dbName))
  (manage q (formatQuery ("DROP DATABASE "%F.database) dbName))
  f
  where
    q = queryParams dbName
