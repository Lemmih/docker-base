module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Char
import qualified Data.Hashable      as Hashable
import           Data.IntMap        (IntMap)
import qualified Data.IntMap        as IntMap
import qualified Data.Set           as Set
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Text.CSV
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Text.Printf

indexFile :: FilePath
indexFile = "sentences.txt"

sentencesDirectory :: FilePath
sentencesDirectory = "sentences"

{-
add file.mp3|.ogg
remove sentence
check
-}
main :: IO ()
main = do
  args <- getArgs
  case args of
    ("add":files)        -> addSentences files
    ["remove", sentence] -> removeSentence sentence
    ["check"]            -> checkDatabase
    ["tts", sentences]   -> getTTS sentences
    _                    -> printUsage

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  hPutStrLn stderr $ "USAGE:"
  hPutStrLn stderr $ "  " ++ prog ++ " add <files>"
  hPutStrLn stderr $ "  " ++ prog ++ " remove <sentence>"
  hPutStrLn stderr $ "  " ++ prog ++ " check"
  exitWith (ExitFailure 1)


checkExit :: ExitCode -> IO ()
checkExit ExitSuccess     = return ()
checkExit (ExitFailure n) = exitWith (ExitFailure n)

getTTS :: FilePath -> IO ()
getTTS sentenceList = do
  db <- readIndex
  mbCSV <- parseCSVFromFile sentenceList
  case mbCSV of
    Left err -> do
      hPutStrLn stderr $ "CSV error: " ++ show err
      exitWith (ExitFailure 1)
    Right csv -> do
      let csvLength = length csv
      forM_ (zip [1..] csv) $ \(nth, [sentence]) -> do
        let clean = cleanSentence sentence
            hash = hashFunction clean
            target = sentencesDirectory </> show hash <.> "mp3"
        case IntMap.lookup hash db of
          Just{} -> return ()
          Nothing | null sentence -> return ()
          Nothing -> do
            printf "[%d/%d] Want to download: %s\n" (nth::Int) csvLength sentence
            checkExit =<< rawSystem "gtts-cli" ["-l","zh-CN","-o",target, sentence]
            checkExit =<< rawSystem "id3tag" ["--comment",sentence,target]
            db <- readIndex
            let db' = IntMap.insert hash clean db
            writeIndex db'
            putStrLn "Done."
            threadDelay (10^6)
  where
    cleanSentence = filter isAlphaNum

addSentences :: [FilePath] -> IO ()
addSentences paths = do
  db <- readIndex
  db' <- foldM addSentence db paths
  writeIndex db'

addSentence :: IntMap String -> FilePath -> IO (IntMap String)
addSentence db path
  | not (isChinese sentence) = return db
  | otherwise = do
    hasMP3 <- doesFileExist mp3Path
    unless hasMP3 $ putStrLn $ "Missing mp3: " ++ mp3Path
    hasOGG <- doesFileExist oggPath
    unless hasOGG $ putStrLn $ "Missing ogg: " ++ oggPath
    unless (hasMP3 && hasOGG) $ exitWith (ExitFailure 1)
    case IntMap.lookup (hashFunction sentence) db of
      Just sentence'
        | sentence == sentence' -> do
          -- putStrLn "Duplicate sentence."
          return db
        | otherwise -> do
          putStrLn $ "Hash failure: " ++ show (sentence, sentence')
          exitWith (ExitFailure 1)
      Nothing -> do
        copyFile mp3Path mp3Dst
        copyFile oggPath oggDst
        let db' = IntMap.insert hash sentence db
        -- putStrLn $ "Added sentence: " ++ show hash ++ " " ++ sentence
        return db'
  where
    sentence = takeBaseName path
    hash     = hashFunction sentence
    mp3Path = dropExtension path <.> "mp3"
    oggPath = dropExtension path <.> "ogg"
    mp3Dst  = sentencesDirectory </> show hash <.> "mp3"
    oggDst  = sentencesDirectory </> show hash <.> "ogg"

removeSentence :: String -> IO ()
removeSentence sentence = return ()

checkDatabase :: IO ()
checkDatabase = do
  db <- readIndex
  let oldHash = length [ v | (k,v) <- IntMap.toList db, hashFunction v /= k ]
  let collisions = IntMap.size db - IntMap.size (IntMap.fromList [ (hashFunction v, v) | v <- IntMap.elems db ])
  let dups = IntMap.size db - Set.size (Set.fromList (IntMap.elems db))
  putStrLn $ "Entries:    " ++ show (IntMap.size db)
  putStrLn $ "Old hashes: " ++ show oldHash
  putStrLn $ "Collisions: " ++ show collisions
  putStrLn $ "Duplicates: " ++ show dups
  forM_ (IntMap.toList db) $ \(hash, v) -> do
    hasMP3 <- doesFileExist (sentencesDirectory </> show hash <.> "mp3")
    unless hasMP3 $ putStrLn $ "Missing mp3: " ++ show hash ++ " " ++ v
    hasOGG <- doesFileExist (sentencesDirectory </> show hash <.> "ogg")
    unless hasMP3 $ putStrLn $ "Missing ogg: " ++ show hash ++ " " ++ v
    when (not hasMP3 && not hasOGG) $ do
      putStrLn $ "All audio files missing. Deleting."
      db <- readIndex
      writeIndex $ IntMap.delete hash db
  return ()

readIndex :: IO (IntMap String)
readIndex = do
  inp <- T.readFile indexFile
  return $ IntMap.fromAscList
    [ case T.words line of
        [key, value] -> (read $ T.unpack key, T.unpack value)
        _            -> error $ "Bad line: " ++ T.unpack line
    | line <- T.lines inp ]

writeIndex :: IntMap String -> IO ()
writeIndex db = writeFile indexFile output
  where
    output = unlines [ unwords [show k, v] | (k, v) <- IntMap.toAscList db ]

hashFunction :: String -> Int
hashFunction = abs . Hashable.hash

isChinese :: String -> Bool
isChinese = all (\c -> generalCategory c == OtherLetter )
