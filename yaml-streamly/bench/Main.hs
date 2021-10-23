{-# LANGUAGE TypeApplications #-}

module Main where

import GHCupTypes

import Criterion.Main
import Data.Yaml
import System.Directory
import System.IO.Temp
import System.FilePath

import qualified System.IO.Strict as SIO
import qualified Data.ByteString as B


-- Our benchmark harness.
main :: IO ()
main = do
  defaultMain [
      env setupEnvDecode $ \ ~(yaml, yamlPath) ->
      bgroup "decode" [ bench "decodeEither'" $ nf (decodeEither' @Value) yaml
                      , bench "decodeFileEither" $ nfIO (decodeFileEither @Value yamlPath)
                      ]
    , envWithCleanup setupEnvEncode cleanupEnvCode $ \ ~(decoded, tmpdir) ->
      bgroup "encode" [ bench "encode" $ nf encode decoded
                      , bench "encodeFile" $ nfIO (encodeFile (tmpdir </> "encoded") decoded)
                      ]
    ]
 where
  setupEnvDecode = do
    let yamlPath = "bench/resources/ghcup-0.0.6.yaml"
    yaml <- B.readFile yamlPath
    pure (yaml, yamlPath)
  setupEnvEncode = do
    decoded <- read @GHCupInfo <$> SIO.readFile "bench/resources/ghcup-0.0.6.yaml.decoded"
    ctmpdir <- getCanonicalTemporaryDirectory
    tmpdir <- createTempDirectory ctmpdir "yaml-bench"
    pure (decoded, tmpdir)
  cleanupEnvCode (_, tmpdir) = do
    removePathForcibly tmpdir


