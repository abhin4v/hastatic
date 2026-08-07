{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Version (showVersion)
import Network.HTTP.Types (status404)
import Network.Wai
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setFdCacheDuration,
    setFileInfoCacheDuration,
    setPort,
    setServerName,
  )
#ifdef ENABLETLS
import qualified Network.Wai.Handler.WarpTLS as TLS
#endif
import Network.Wai.Middleware.Static
import Paths_hastatic (version)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (lookupEnv)
#ifdef ENABLETLS
import System.Exit (die)
#endif
import Text.Read (readMaybe)

data PathInfo = PathDir | PathFile | PathMissing

fileType :: IORef (HashMap.HashMap FilePath PathInfo) -> FilePath -> IO PathInfo
fileType cache filePath = do
  cached <- HashMap.lookup filePath <$> readIORef cache
  case cached of
    Just info -> return info
    Nothing -> do
      info <- statPath filePath
      atomicModifyIORef' cache (\m -> (HashMap.insert filePath info m, ()))
      return info
  where
    statPath p = do
      isDir <- doesDirectoryExist p
      if isDir
        then return PathDir
        else do
          isFile <- doesFileExist p
          return $ if isFile then PathFile else PathMissing

addSecureHeaders :: Middleware
addSecureHeaders = modifyResponse $ mapResponseHeaders (++ secureHeaders)
  where
    secureHeaders =
      [ ("Referrer-Policy", "strict-origin-when-cross-origin"),
        ("X-Frame-Options", "SAMEORIGIN"),
        ("X-XSS-Protection", "1; mode=block")
      ]

indexHTML :: IORef (HashMap.HashMap FilePath PathInfo) -> T.Text -> Middleware
indexHTML cache indexFile app req respond =
  fixPath (pathInfo req) >>= flip app respond . (\path -> req {pathInfo = path})
  where
    fixPath path =
      case List.dropWhileEnd T.null path of
        [] -> return [indexFile]
        (".well-known" : rest) -> return $ ".well-known" : rest
        path' -> do
          let filePath = T.unpack $ T.intercalate "/" path'
          info <- fileType cache filePath
          return $
            case info of
              PathDir -> path' ++ [indexFile]
              _ -> path'

notFoundHandler :: FilePath -> Application
notFoundHandler notFoundFile _ respond =
  respond . responseLBS status404 [("Content-Type", getMimeType notFoundFile)] =<< BL.readFile notFoundFile

#ifdef ENABLETLS
data TLS = Okay FilePath FilePath | Error String | None

getTLSSettings :: IO TLS
getTLSSettings = do
  tlsCertFile <- lookupEnv "TLS_CERT_FILE"
  tlsKeyFile <- lookupEnv "TLS_KEY_FILE"

  case (tlsCertFile, tlsKeyFile) of
    (Nothing, Nothing) -> return None
    (Just cert, Just key) -> return $ Okay cert key
    _ -> return $ Error "Certificate file or Key file is missing"
#endif

application :: [FilePath] -> IO Application
application excludedPaths = do
  notFoundFile <- fromMaybe "404.html" <$> lookupEnv "NF_FILE"
  indexFile <- T.pack . fromMaybe "index.html" <$> lookupEnv "IDX_FILE"
  cache <- initCaching PublicStaticCaching
  pathCache <- newIORef HashMap.empty
  return
    . addSecureHeaders
    . indexHTML pathCache indexFile
    . staticPolicyWithOptions (defaultOptions {cacheContainer = cache}) polcy
    $ notFoundHandler notFoundFile
  where
    noDot p =
      List.isPrefixOf ".well-known/" p
        || not (any (T.isPrefixOf ".") $ T.splitOn "/" $ T.pack p)
    polcy = predicate noDot >-> predicate (not . flip elem excludedPaths)

main :: IO ()
main = do
  mPort <- lookupEnv "PORT"
  let port = fromMaybe 8080 (readMaybe =<< mPort)
  let settings =
        setPort port
          . setFdCacheDuration 10
          . setFileInfoCacheDuration 10
          . setServerName ("hastatic-" <> C8.pack (showVersion version))
          $ defaultSettings
#ifdef ENABLETLS
  tlsSettings <- getTLSSettings
  case tlsSettings of
    Okay certFile keyFile -> do
      app <- application [certFile, keyFile]
      putStrLn $ "Starting HTTPS server on port: " <> show port
      TLS.runTLS (TLS.tlsSettings certFile keyFile) settings app
    None -> startServer port settings
    Error msg -> die $ "Error starting server: " <> msg
#else
  startServer port settings
#endif
  where
    startServer port settings = do
      app <- application []
      putStrLn $ "Starting HTTP server on port: " <> show port
      runSettings settings app
