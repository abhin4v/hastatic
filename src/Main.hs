{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
import qualified Data.List as List
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Middleware.Static
import Network.HTTP.Types (status404)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

indexHTML :: T.Text -> Middleware
indexHTML indexFile app req respond =
  let path = pathInfo req
  in flip app respond $ req { pathInfo = fixPath path }
  where
    fixPath path = case path of
      []     -> [indexFile]
      [""]   -> [indexFile]
      [x]    -> if "." `T.isInfixOf` x then [x] else [x, indexFile]
      (x:xs) -> x : fixPath xs

notFoundHandler notFoundFile _ respond = respond $
  responseFile status404 [("Content-Type", "text/html")] notFoundFile Nothing

main = do
  mPort <- lookupEnv "PORT"
  let port = fromMaybe 3000 (readMaybe =<< mPort)
  mNotFoundFile <- lookupEnv "NF_FILE"
  let notFoundFile = fromMaybe "404.html" mNotFoundFile
  mIndexFile <- lookupEnv "IDX_FILE"
  let indexFile = T.pack $ fromMaybe "index.html" mIndexFile

  cache <- initCaching PublicStaticCaching
  putStrLn $ "Starting server on port: " <> show port
  run port
    $ indexHTML indexFile
    $ staticPolicy' cache (predicate noDot)
    $ notFoundHandler notFoundFile
  where
    noDot = not . List.isPrefixOf "."
