{-# LANGUAGE BangPatterns #-}

{-

Display size of current directory and its sub-directories;
a very=basic du.

  ghc --make -Wall -O2 -o mydu MyDu.hs

-}

module Main where

import System.Exit
import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified System.IO.Error as E

import Text.Printf 

import Data.Maybe (isJust, catMaybes)
import Data.Ord (comparing)
import Data.List (sortBy)
import Control.Monad (unless, when)

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
                 (return . const Nothing)
    where
      processStatus fs | isDirectory fs    = Directory
                       | isSymbolicLink fs = Other
                       | otherwise         = File


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
            zs = map (\(a,Just b) -> (a,b)) ys

            conv (fp, Directory) inDir = inDir { dirs = fp : dirs inDir }
            conv (fp, File)      inDir = inDir { files = fp : files inDir }
            conv (fp, Other)     inDir = inDir { others = fp : others inDir }

        return $ foldr conv (emptyDir dname) zs

{-
Given a directory and filename, return the length of the file
in bytes. This is taken from Real World Haskell (chapter 9, page 221)
since the
combination of fmap filesize (getFileStatus (dname </> fname))
using System.Pisux.Files seems to return garbage results.

getFileSize :: FilePath -> FilePath -> IO (Either IOError Integer)
getFileSize dname fname = E.try $ fmap (fromIntegral . fileSize) (getFileStatus (dname </> fname))

ignoreIOError :: IOError -> IO (Maybe a)
ignoreIOError = const (return Nothing)

getFileSize :: FilePath -> FilePath -> IO (Maybe Integer)
getFileSize dname fname = handle ignoreIOError $
  bracket (openFile (dname </> fname) ReadMode) hClose $ fmap Just . hFileSize

It appears that getSymbolicLinkStatus works whereas getFileStatus
doesn't on my laptop ...
-}

getFileSize :: FilePath -> FilePath -> IO (Maybe Integer)
getFileSize dname fname = fmap (either (const Nothing) Just) $
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
                 orderedDirs = reverse $ sortBy (comparing snd) dList
                 eLine = putStrLn ""
             mapM_ dispDir orderedDirs
             unless (fs == 0) $ eLine >> dispLine "+ files" fs
             eLine
             putStrLn $ "Total: " ++ prettify (fs + sum dss)
             exitSuccess

usage :: IO ()
usage = getProgName >>= \n -> 
        putStrLn ("Usage: " ++ n ++ " [-a] [directory]") >> 
        exitFailure

main :: IO ()
main = do
  args <- getArgs
  if null args
    then listSizes False "."
    else if length args > 2
       then usage
       else do
         let flag = head args == "-a"
             dname | length arfs == 2 = (head . tail) args
                   | flag             = "."
                   | otherwise        = head args
         when (length args == 2 && not flag) usage
         listSizes flag dname
         




