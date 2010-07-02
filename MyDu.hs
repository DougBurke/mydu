{-

Display size of current directory and its sub-directories;
a very=basic du.

  ghc -Wall -o mydu MyDu.hs

-}

module Main where

import System.Exit
import System.Environment
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified System.IO.Error as E

import Data.Maybe (isJust)
import Data.Either (rights)

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
      processStatus fs = if isDirectory fs
                            then Directory
                            else if isSymbolicLink fs
                                 then Other
                                 else File


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
in bytes.
-}
getFileSize :: FilePath -> FilePath -> IO (Either IOError Integer)
getFileSize dname fname = E.try $ fmap (fromIntegral . fileSize) (getFileStatus (dname </> fname))

{-
Return the number of bytes occupied by the files in the
current directory. We return those files for which we
can not find a size.
-}
getFileSizes :: FilePath -> [FilePath] -> IO Integer
getFileSizes dName = fmap (sum . rights) . mapM (getFileSize dName) 

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
             fs <- getFileSizes curName (files dc)
             dss <- mapM (getDirSize curName) (dirs dc)
             return $ fs + sum dss
  
listSizes :: FilePath -- ^ directory to use
          -> IO ()
listSizes dname = do
  edc <- ls dname
  case edc of
    Left e -> do
             putStrLn $ "Unable to get directory contents:\n  " ++ show e
             exitFailure
    Right dc -> do
             fs <- getFileSizes dname (files dc)
             dss <- mapM (getDirSize dname) (dirs dc)
             putStrLn $ "Size of directory: " ++ dname
             let dispDir (dn,ds) = putStrLn $ "  " ++ dn ++ ": " ++ show ds
             mapM_ dispDir (zip (dirs dc) dss)
             putStrLn $ "  + files: " ++ show fs
             putStrLn $ "Total: " ++ show (fs + sum dss)
             exitSuccess

usage :: IO ()
usage = getProgName >>= \n -> 
        putStrLn ("Usage: " ++ n ++ " [directory]") >> 
        exitFailure

main :: IO ()
main = do
  args <- getArgs
  if null args
    then listSizes "."
    else if length args == 1
       then listSizes (head args)
       else usage
         



