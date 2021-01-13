{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Test.Hspec

import           Control.Exception      (Exception (fromException))
import           Control.Exception.Base (IOException, SomeException)
import           Data.Maybe             (isJust)
import           Data.Pool
import           Debug.Trace            (traceShow)

main :: IO ()
main = hspec $ do
  describe "acquiring resources" $ do
    it "timeout exception is thrown if acquiring takes too long" $ do
      acquiringThrowsWhenTimeoutExpired `shouldThrow` anyException

mkPool :: IO (Pool ())
mkPool =
  createPool
    (pure ())
    (const (pure ()))
    stripes
    idleTime
    maxResources
    timeout
  where
    stripes = 1
    idleTime = 1000
    maxResources = 1
    timeout = Just 1

acquiringThrowsWhenTimeoutExpired :: IO ()
acquiringThrowsWhenTimeoutExpired = do
  pool <- mkPool
  _ <- takeResource pool
  _ <- takeResource pool
  pure ()

