{-# LANGUAGE RecordWildCards #-}

module Main where

import KeysGenCommandLine
import Crypto.Sign.Ed25519
import Data.Functor
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base16 as Base16
import Data.Monoid
import Options.Applicative
import System.Directory
import System.Random

main :: IO ()
main = generateSks =<< execParser parseArguments

keysDir :: String
keysDir = "keys"

distributionFile :: String
distributionFile = "Distribution.keys"

initialAmountDistribution :: [Int]
initialAmountDistribution = (*10) <$> randomRs (10, 200) (mkStdGen 10)

skToKeyName :: SecretKey -> String
skToKeyName =  toKeyName . unSecretKey

pkToKeyName :: PublicKey -> String
pkToKeyName = toKeyName . unPublicKey

toKeyName :: BSC.ByteString -> String
toKeyName = BSC.unpack . Base16.encode

writeSecretKeys :: [SecretKey] -> IO ()
writeSecretKeys sks =
  void $ sequence $ (\sk -> writeSecretKey (keysDir <> "/" <> (skToKeyName sk)) sk) <$> sks

mkDistributionFile :: [PublicKey] -> IO String
mkDistributionFile sks = do
  let dFileContent = foldMap (\(keyName, amount) -> keyName <> " " <> show amount <> "\n") (zip (pkToKeyName <$> sks) initialAmountDistribution)
  BSC.writeFile distributionFile (BSC.pack dFileContent)
  pure dFileContent

generateSks :: KeyGenConfig -> IO ()
generateSks KeyGenConfig {..} = do
  cd <- getCurrentDirectory
  keyPairs <- replicateM numberOfKeys createKeypair
  let (pks, sks) = unzip keyPairs
  writeSecretKeys sks
  dFile <- mkDistributionFile pks
  allFiles <- listDirectory (cd <> "/" <> keysDir)
  let keys = filter (\a -> length a == 128) allFiles
  putStrLn $ "Distribution file: " <> cd <> "/" <> distributionFile
  putStrLn dFile
  putStrLn $ "Keys successfully written to " <> cd <> "/" <> keysDir
  mapM_ putStrLn keys
  putStrLn $ "Total number of keys: " <> show (length keys)

writeSecretKey :: FilePath -> SecretKey -> IO ()
writeSecretKey filePath (SecretKey sk) = BSC.writeFile filePath sk
