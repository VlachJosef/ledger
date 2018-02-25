{-# LANGUAGE RecordWildCards #-}

module Main where

import           Crypto.Sign.Ed25519
import qualified Data.ByteString.Char8  as BSC
import qualified Data.ByteString.Base16 as Base16
import           Data.Functor
import           Data.Monoid
import           KeysGenCommandLine
import           Options.Applicative
import           System.Directory
import           System.Random
import           System.FilePath ((</>))
import           Text.Printf

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
  void $ sequence $ (\sk -> writeSecretKey (keysDir </> (skToKeyName sk)) sk) <$> sks

mkDistributionFile :: [PublicKey] -> IO String
mkDistributionFile sks = do
  let dFileContent = foldMap (\(keyName, amount) -> keyName <> " " <> show amount <> "\n") (zip (pkToKeyName <$> sks) initialAmountDistribution)
  BSC.writeFile distributionFile (BSC.pack dFileContent)
  pure dFileContent

getSecretKeys :: IO [FilePath]
getSecretKeys = do
  cd <- getCurrentDirectory
  let skDir = cd </> keysDir
  allFiles <- listDirectory skDir
  pure $ (\skFile -> skDir </> skFile) <$> filter (\skFile -> length skFile == 128) allFiles

purgeSecretKeys :: IO ()
purgeSecretKeys = do
  sks <- getSecretKeys
  (void . sequence) (removeFile <$> sks)

generateSks :: KeyGenConfig -> IO ()
generateSks KeyGenConfig {..} = do
  purgeSecretKeys
  let keyPairGen = createKeypairFromSeed <$> seeds
  let keyPairs = take numberOfKeys  keyPairGen
  let (pks, sks) = unzip keyPairs
  writeSecretKeys sks
  dFile <- mkDistributionFile pks
  cd <- getCurrentDirectory
  putStrLn $ "New distribution file entries: " <> cd </> distributionFile
  putStrLn dFile
  putStrLn $ "New keys successfully written to " <> cd </> keysDir
  mapM_ putStrLn (skToKeyName <$> sks)
  putStrLn $ "Total number of keys: " <> show numberOfKeys

writeSecretKey :: FilePath -> SecretKey -> IO ()
writeSecretKey filePath (SecretKey sk) = BSC.writeFile filePath sk

allStrings :: [String]
allStrings = [ c : s | s <- "" : allStrings, c <- ['A'..'Z']]

seeds :: [BSC.ByteString]
seeds = BSC.pack .reverse <$> printf "%032s" <$> allStrings
