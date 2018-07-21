{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
import qualified Data.List as List
import qualified Data.Text as T
import Network.Wai
import qualified Network.Wai.Handler.WarpTLS as TLS
import Network.Wai.Middleware.Static
import Network.HTTP.Types (status404)
import Network.Wai.Handler.Warp (run, defaultSettings, setPort)
import System.Exit (die)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data TLS = Okay TLS.TLSSettings | Error String | None

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

notFoundHandler :: FilePath -> Application
notFoundHandler notFoundFile _ respond = respond $
  responseFile status404 [("Content-Type", "text/html")] notFoundFile Nothing

getTLSSettings :: IO TLS
getTLSSettings = do
  tlsCertFile <- lookupEnv "TLS_CERT_FILE"
  tlsKeyFile  <- lookupEnv "TLS_KEY_FILE"

  case (tlsCertFile, tlsKeyFile) of
    (Nothing, Nothing)    -> return None
    (Just cert, Just key) -> return $ Okay $ TLS.tlsSettings cert key
    _                     -> return $ Error "Certificate file or Key file is missing"

application :: [FilePath] -> IO Application
application excludedPaths = do
  notFoundFile <- fromMaybe "404.html" <$> lookupEnv "NF_FILE"
  indexFile    <- T.pack . fromMaybe "index.html" <$> lookupEnv "IDX_FILE"
  cache        <- initCaching PublicStaticCaching
  return
    . indexHTML indexFile
    . staticPolicy' cache polcy
    . notFoundHandler
    $ notFoundFile
  where
    noDot = not . List.isPrefixOf "."
    polcy = predicate noDot >-> predicate (not . flip elem excludedPaths)

main :: IO ()
main = do
  mPort       <- lookupEnv "PORT"
  let port    = fromMaybe 3000 (readMaybe =<< mPort)
  tlsSettings <- getTLSSettings

  case tlsSettings of
    Okay tls  -> do
      app <- application [TLS.certFile tls, TLS.keyFile tls]
      putStrLn $ "Starting HTTPS server on port: " <> show port
      TLS.runTLS tls (setPort port defaultSettings) app
    None      -> do
      app <- application []
      putStrLn $ "Starting HTTP server on port: " <> show port
      run port app
    Error msg ->
      die $ "Error starting server: " <> msg
