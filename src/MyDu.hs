{-# LANGUAGE BangPatterns #-}

{-

Display size of current directory and its sub-directories;
a very-basic du.

-}

module Main where

-- import qualified System.IO.Error as E
import qualified Control.Exception as E
import qualified System.Info as SI

-- import qualified PackageInfo_mydu as P
import qualified Paths_mydu as P

import System.Exit
import System.Environment
import System.Directory hiding (getFileSize, isSymbolicLink)
import System.FilePath
import System.Posix.Files

import Text.Printf 

import Control.Arrow (second)
import Control.Monad (unless)

import Data.List (sortOn)
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Ord (Down(Down))
import Data.Version (showVersion)

{-
Given a directory, find all its components:

  - split up into files, directories, and "others"
  - calculate the size of the files
  - loop through each directory, and repeat above
  - add in the size of each directory to the files in
    the directory and return

-}

-- Having trouble compiling against split-0.1.2 so doing it manually
-- import qualified Data.List.Split as S

-- | Simple information about a given FilePath
data FileType = Directory -- ^ A directory
              | File -- ^ something with a size (file or executable, but not a link)
              | Other -- ^ None of the above (at the moment, just links)
                deriving (Eq, Show)

data DirContents = DirContents {
      dirName :: FilePath,
      dirs :: [FilePath],
      files :: [FilePath],
      others :: [FilePath]
    }
                   deriving (Eq, Show)

emptyDir :: FilePath -> DirContents
emptyDir fp = DirContents fp [] [] []

-- | Return the type of a file, or Nothing if there
-- was an error accessing the information.
--
getFileType :: FilePath -> IO (Maybe FileType)
getFileType fp = E.catch (fmap (Just . processStatus) (getSymbolicLinkStatus fp))
                 handleIOError
    where
      processStatus fs | isDirectory fs    = Directory
                       | isSymbolicLink fs = Other
                       | otherwise         = File

      handleIOError :: E.IOException -> IO (Maybe a)
      handleIOError = return . const Nothing

{-
This excludes "." and ".."
-}

ls :: FilePath -> IO (Either IOError DirContents)
ls dname = E.try ls'
    where
      ls' = do
        fs <- getDirectoryContents dname
        ts <- mapM (getFileType . (dname </>)) fs

        -- note we filter out '.' and '..' here
        let xs = filter (isJust . snd) $ zip fs ts
            ys = filter ((`notElem` [".", ".."]) . fst) xs
            zs = map (second fromJust) ys

            conv (fp, Directory) inDir = inDir { dirs = fp : dirs inDir }
            conv (fp, File)      inDir = inDir { files = fp : files inDir }
            conv (fp, Other)     inDir = inDir { others = fp : others inDir }

        return $ foldr conv (emptyDir dname) zs

{-
Given a directory and filename, return the length of the file
in bytes. This is taken from Real World Haskell (chapter 9, page 221)
since the
combination of fmap filesize (getFileStatus (dname </> fname))
using System.Posix.Files seems to return garbage results.

getFileSize :: FilePath -> FilePath -> IO (Either IOError Integer)
getFileSize dname fname = E.try $ fmap (fromIntegral . fileSize) (getFileStatus (dname </> fname))

ignoreIOError :: IOError -> IO (Maybe a)
ignoreIOError = const (return Nothing)

getFileSize :: FilePath -> FilePath -> IO (Maybe Integer)
getFileSize dname fname = handle ignoreIOError $
  bracket (openFile (dname </> fname) ReadMode) hClose $ fmap Just . hFileSize

It appears that getSymbolicLinkStatus works whereas getFileStatus
doesn't on my laptop ...

I am not bothering to check if the System.Directory.getFileSize routine
works as we need it (it wasn't available when the code was written).
-}

getFileSize :: FilePath -> FilePath -> IO (Maybe Integer)
getFileSize dname fname =
  let handleIOError :: Either E.IOException Integer -> Maybe Integer
      handleIOError = either (const Nothing) Just

  in fmap handleIOError $
     E.try $ fmap (fromIntegral . fileSize) (getSymbolicLinkStatus (dname </> fname))

{-
Return the number of bytes occupied by the files in the
current directory.
-}
getFileSizes :: FilePath -> [FilePath] -> IO Integer
getFileSizes dName = fmap (sum . catMaybes) . mapM (getFileSize dName) 

{-
Calculate the size of a directory, including its sub-components.
Any errors (e.g. files being changed during processing/unable to be read)
are silently ignored, so the return value could be 0.
-}
getDirSize :: FilePath -> FilePath -> IO Integer
getDirSize pName dName = do
  let curName = pName </> dName
  edc <- ls curName
  case edc of
    Left _ -> return 0
    Right dc -> do 
             !fs <- getFileSizes curName (files dc)
             !dss <- mapM (getDirSize curName) (dirs dc)
             return $ fs + sum dss

{-
Convert a number of bytes into a human-readable string.
Given the rounding we are doing, can get results like 1024.00 Kb
when nb =1024 * 1024 - 10 say.  Could clean this up.
-}
prettify :: Integer -> String
prettify nb | nb == 0 = "empty"
            | nb == 1 = "1 byte"
            | nb < 1024 = printf "%4d bytes" nb
            | nb < 1024 * 1024 = printf "%7.2f Kb" (nbf 1)
            | nb < 1024 * 1024 * 1024 = printf "%7.2f Mb" (nbf 2)
            | otherwise = printf "%7.2f Gb" (nbf 3) -- can overflow but that's okay
    where
      nbf p = (fromIntegral nb :: Float) / (1024**p)

listSizes :: Bool -- ^ True if empty directories should be included in the output
          -> FilePath -- ^ directory to use
          -> IO ()
listSizes flag dname = do
  edc <- ls dname
  case edc of
    Left e -> do
             putStrLn $ "Unable to get directory contents:\n  " ++ show e
             exitFailure
    Right dc -> do
             !fs <- getFileSizes dname (files dc)
             !dss <- mapM (getDirSize dname) (dirs dc)
             putStrLn $ "Size of directory: " ++ dname
             let dispLine n s = putStrLn $ printf "  %-50s  %s" n (prettify s)
                 dispDir (dn,ds) = dispLine dn ds
                 dList = filter ((||) flag . (/=) 0 . snd) (zip (dirs dc) dss)
                 orderedDirs = sortOn (Down . snd) dList
                 eLine = putStrLn ""
             mapM_ dispDir orderedDirs
             unless (fs == 0) $ eLine >> dispLine "+ files" fs
             eLine
             putStrLn $ "Total: " ++ prettify (fs + sum dss)
             exitSuccess

usage :: IO ()
usage = getProgName >>= \n -> 
        putStrLn ("Usage: " ++ n ++ " [-a|-h|-v] [directory]") >> 
        exitFailure

reportVersion :: IO ()
reportVersion = do
  name <- getProgName
  putStrLn (name <> ": v" <> showVersion P.version <> " (" <>
           SI.compilerName <> " " <> showVersion SI.fullCompilerVersion <>
           " " <> SI.os <> " " <> SI.arch <> ")")
  exitSuccess


data Args = Args {
     help :: Bool,
     version :: Bool,
     allFiles :: Bool,
     location :: String }

defArgs :: Args
defArgs = Args { help = False, version = False, allFiles = False, location = "." }


-- Muddle through without a proper parser for now
processArgs :: [String] -> Maybe Args
processArgs [] = Just defArgs
processArgs ["-h"] = Just $ defArgs { help = True }
processArgs ["-a"] = Just $ defArgs { allFiles = True }
processArgs ["-v"] = Just $ defArgs { version = True }
processArgs [loc] = Just $ defArgs { location = loc }
processArgs ["-a", loc] = Just $ defArgs { allFiles = True, location = loc }
processArgs ["-h", loc] = Just $ defArgs { help = True, location = loc }  -- could make this an error
processArgs ["-v", loc] = Just $ defArgs { version = True, location = loc }  -- could make this an error
processArgs _ = Nothing


main :: IO ()
main = do
  mArgs <- processArgs <$> getArgs
  case mArgs of
    Just args -> do
      if help args
      then usage
      else if version args
           then reportVersion
           else listSizes (allFiles args) (location args)

    Nothing -> usage
