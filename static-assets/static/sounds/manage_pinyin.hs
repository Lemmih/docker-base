{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Char
import           Data.Chinese.CCDict
import qualified Data.Hashable       as Hashable
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap
import           Data.List
import qualified Data.Set            as Set
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

pinyinDirectory :: FilePath
pinyinDirectory = "pinyin"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["fix_tones"]        -> fixTones
    -- ["fix_ogg"]          -> fixOgg
    ["fetch", wordList]  -> fetchMissing wordList
    _                    -> printUsage

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  hPutStrLn stderr $ "USAGE:"
  hPutStrLn stderr $ "  " ++ prog ++ " fix_tones"
  -- hPutStrLn stderr $ "  " ++ prog ++ " remove <sentence>"
  -- hPutStrLn stderr $ "  " ++ prog ++ " check"
  exitWith (ExitFailure 1)

fetchMissing :: FilePath -> IO ()
fetchMissing wordList = do
  inp <- T.lines <$> T.readFile wordList
  forM_ inp $ \word ->
    case lookupMatch word of
      Nothing -> hPutStrLn stderr $ "No match for: " ++ T.unpack word
      Just e  -> do
        let unambiguous = length (nub (map (T.toLower . variantPinyin) (entryVariants e))) == 1
        forM_ (entryVariants e) $ \Variant{..} -> do
          let key = map toLower $ filter (not.isSpace) (T.unpack variantPinyin)
              target = pinyinDirectory </> key <.> "mp3"
          exists <- doesFileExist target
          unless exists $
            if unambiguous
              then do
                putStrLn $ "Want to download: " ++ T.unpack variantSimplified ++ " to " ++ key
                status <- rawSystem "gtts-cli" ["-l","zh-CN","-o",target, T.unpack word]
                case status of
                  ExitSuccess -> putStrLn "Done."
                  ExitFailure n -> exitWith (ExitFailure n)
                rawSystem "id3v2" ["-c","tts",target]
                threadDelay (10^6)
              else
                putStrLn $ "Ambiguous: " ++ T.unpack variantSimplified ++ " to " ++ key


fixTones :: IO ()
fixTones = do
  ls <- getDirectoryContents pinyinDirectory
  forM_ ls $ \file ->
    when (takeExtension file `elem` [".mp3", ".ogg"]) $ do
      let newFile = lowerPinyinTones file
      unless (file == newFile) $ do
        putStrLn $ "Move: " ++ file ++ " -> " ++ newFile
        renameFile (pinyinDirectory </> file) (pinyinDirectory </> newFile)

lowerPinyinTones :: String -> String
lowerPinyinTones [] = []
lowerPinyinTones [x] = [x]
lowerPinyinTones (x:y:xs)
  | isDigit y, Just idx <- elemIndex x vowals =
    (toneMap !! (yDigit-1))!!idx : lowerPinyinTones xs
  | Just idx <- elemIndex x vowals =
      (toneMap !! 4)!!idx : lowerPinyinTones (y:xs)
  | otherwise = x:lowerPinyinTones (y:xs)
  where
    yDigit = digitToInt y
    vowals = "aeiouvAEIO"
    toneMap =
      [['ā','ē','ī','ō','ū','ǖ','Ā','Ē','Ī','Ō']
      ,['á','é','í','ó','ú','ǘ','Á','É','Í','Ó']
      ,['ǎ','ě','ǐ','ǒ','ǔ','ǚ','Ǎ','Ě','Ǐ','Ǒ']
      ,['à','è','ì','ò','ù','ǜ','À','È','Ì','Ò']
      ,"aeiouüAEIO"]
