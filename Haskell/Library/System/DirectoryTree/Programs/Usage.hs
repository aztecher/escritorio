module Usage where
import System.Directory.Tree
import System.IO

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Control.Lens
import Data.Maybe

main = putStrLn "directory-tree"

readDirectoryTest = do
  anchor:/dtree <- build "../"
  putStrLn . show $ anchor:/dtree

-- writeDirectoryでディレクトリからファイルまで全て作成する.
handmadeDirectory = writeDirectory
  $ "./" :/ Dir {
    name = "Tux",
    contents = [File {
      name = "README",
      file = "Welcome!"
    }, Dir {
      name = "Child",
      contents = [File {
        name ="Hoge.rst",
        file = "Test\n======="
      }]
    }]
  }

-- concat directory
concatDirectory = do
  b:/dt <- readDirectory "../"
  let f = F.concat dt
  return $ b:/File {name = "ALL_TEXT", file = f}

-- 全てのファイルの内容を集計したファイル "ALL_TEXT" を作る.
concatAndWriteDirectory = do
  b:/dt <- readDirectory "../"
  let f = F.concat dt
  writeDirectory $  b:/File {name = "ALL_TEXT", file = f}

defaultPath = do
  b:/dt <- readDirectoryWith return "./"
  putStrLn . show $ b:/dt

-- zipPaths で fileはファイルのフルパスとファイルの内容のタプルになる.
allFullPath = do
  b:/dt <- readDirectoryWith return "./"
  putStrLn . show $ zipPaths $ b:/dt

checkFold = do
  b:/dt <- readDirectory "../"
  putStrLn . show $ dt

showDirTree = do
  b:/dt <- readDirectoryWith (\fp -> putStrLn . show $ fp) "./"
  putStrLn . show $ (b:/dt)

-- specified
specifiedDirTree = do
  b:/dt <- readDirectoryWith (\fp -> if fp == "./Usage.hs" then putStrLn "True" else putStrLn "False") "./"
  putStrLn . show $ (b:/dt)


-- Directoryを指定して開く場合は, openDirectoryで指定
-- Fileを開くわけではないのが注意事項か...?
openDirectoryTest :: IO (AnchoredDirTree Handle)
openDirectoryTest = openDirectory "./Tux/Child" ReadMode


filterD = do
  b:/dt <- readDirectoryWith return "./"
  let fdt = filterDir ptmatch dt
  putStrLn . show $ dt
  putStrLn . show $ fdt
    where
      ptmatch :: DirTree a -> Bool
      ptmatch (Dir _ _) = True
      ptmatch _ = False

filterF = do
  b:/dt <- readDirectoryWith return "./"
  let fdt = filterDir ptmatch dt
  putStrLn . show $ dt
  putStrLn . show $ fdt
    where
      ptmatch :: DirTree a -> Bool
      ptmatch (File _ _) = True
      ptmatch _ = False

filterFN = do
  b:/dt <- readDirectoryWith return "./"
  let fdt = filterDir ptmatch dt
  putStrLn . show $ dt
  putStrLn . show $ fdt
    where
      ptmatch :: DirTree a -> Bool
      ptmatch (File name _) = if name == "Hoge.rst" then True else False
      ptmatch _ = False

ritchFilter = do
  b:/dt <- readDirectoryWith return "./"
  let fdt = filterDir ritchmatch dt
  putStrLn . show $ dt
  putStrLn . show $ fdt
    where
      ritchmatch (Dir name contents) =
        if head(contents)^._file == "Usage.hs" then True else False
      ritchmatch _ = False

-- dropTo は現在のディレクトリのマッチングみたいなもん.
-- ディレクトリ名でマッチングしたらそのサブディレクトリ(DirTree)を返す.
-- ファイル名でマッチングしたらファイル(File)を返す
-- Maybeモナドなのでバインドで処理を連続できる
-- dropToを連続でつなげることももちろん可能

-- 各ファイルへの絶対パスアクセスは, zipPathsとflattenDirでうまく取得できそう.
fileAccess :: IO ()
fileAccess = do
  (b:/dt) <- readDirectoryWith return "./"
  let fileList = flattenDir $ zipPaths (b:/dt)
  putStrLn . show $ fileList

findfile :: IO ()
findfile = do
  (b:/dt) <- readDirectoryWith return "./"
  let fileList = flattenDir $ zipPaths (b:/dt)
  let filterd = fmap findfile fileList
  putStrLn . show $ filterd
    where
      findfile (File name file) = if (snd $ file) == "./Usage.hs" then True else False
      findfile _ = False

-- contents の一覧が得られる
-- (同様の効果が, F.toList dt で得られる)
findfilefold :: IO ()
findfilefold = do
  (b:/dt) <- readDirectoryWith return "./"
  putStrLn . show $ (b:/dt)
  putStrLn . show $ dt
  -- let allname = F.foldr (++) "" dt
  let allname = F.foldr (:) [] dt
  putStrLn . show $ allname
  -- F.foldr showName "" dt
    -- where
    --   showName :: DirTree a -> IO ()
    --   showName (Dir name contents) = (++)

-- findfiletraverse :: IO ()
findfiletraverse = do
  (b:/dt) <- readDirectoryWith return "./"
  putStrLn . show $ (b:/dt)
  putStrLn . show $ dt
  T.traverse (\n -> print n) dt
    -- where
    --   showDir (Dir name contents) = putStrLn . show $ name

selectDirectory = do
  (b:/dt) <- readDirectoryWith return "./"
  let tux = dropTo "Tux" (b:/dt)
      child = dropTo "Child" =<< dropTo "Tux" (b:/dt)
      tux' = fromJust tux
  putStrLn . show $ dt
  putStrLn . show $ tux
  putStrLn . show $ child
  if isJust tux
    then putStrLn . show $ fromJust tux
    else putStrLn "Not Just!"


