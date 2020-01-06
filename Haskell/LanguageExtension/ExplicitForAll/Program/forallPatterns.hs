{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
-- ***** GHCのforallと多相性への対処 *****
--
-- GHCの `forall` には一見複数の意味があるように思われる
-- * 関数シグネチャの `forall`
-- * `RankNTypes` による `forall`
-- * ExistentialQuantification` による `forall`
--
-- しかし基本的な考え方は共通している


-- =============================
-- 関数シグネチャとしてのforall
-- =============================
--
-- Haskellで関数の型シグネチャに現れる変数は, 暗黙に量化される.
-- (より詳細な説明は, ...に記載する)
--
-- > id :: a -> a
--
-- という関数は正確には, `forall` を用いて以下のように表現される
-- (`forall` 明示には ExplicitForAll`
--  もしくは, それをimplyする言語拡張を指定する必要がある)
--
-- > id :: forall a. (a -> a)
--
-- そしてこの a が具体化されるのは関数適用時である.
--
-- > main = putStrLn $ id "Hello, type variables!"
--
-- ここで関数, `id` を "Hello, type variables!" に対して適用すると
-- その適用の応じて型変数が具体的な型に具体化される.
-- ここでは, `String` と推論される

main1 = do
  putStrLn $ id "Hello, type variables!" -- id を `String` に適用
  print $ id 42 -- id を `Integer` に適用

-- こうして
-- 「複数の型に対応する関数を一つにまとめられる」
-- ことが parametric polymorphism の利点である.


-- さて, 他の言語のジェネリクスでは, 型変数を `<>` やら `[]` やらで
-- 囲んで導入する.
-- これらは, クラスの頭についていたらオブジェクト生成時に具体化され,
-- メソッドの頭についていたらメソッド呼び出し時に具体化される
--
-- ランク1の型の `forall` はそれと同じものである
-- Haskell では冠頭の `forall` が省略できてしまうので, 型変数の導入が曖昧に見えてしまうかもしれない.

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = f x : mapList f xs

main2 = print $ mapList show [1, 2, 3, 4]


-- ============================
-- RankNTypes による forall
-- ============================
-- 
-- 言語拡張 `RankNTypes` を指定すると, 任意のランクの型を持つ関数を定義できる.
-- 例えば `forall` がついた関数を渡す高階関数を定義できる

main3 = putStrLn $ f show 42 True

f :: forall a b. (Show a, Show b)
  => (forall s. Show s => s -> String)
  -> a -> b -> String
f toString a b = toString a ++ " " ++ toString b

-- 型変数 `a`, `b` が具体化されるのは `f` の関数適用の時, `s` の具体化は引数で渡した関数 `toString` を適用する度に行われる
-- 比較のために, 全ての `forall` を冠頭部に持ってきた関数を見てみる

-- -- これは型エラー
-- g :: forall a b s. (Show a, Show b)
--   => (Show s => s -> String)
--   -> a -> b -> String
-- g toString a b = toString a ++ " " ++ toString b

-- この関数は型エラーを発生する.
-- `g` を関数適用した時点で, `a`, `b`, `s` は全て決定するのに,
-- `a` と `b` と `s` は同じ型とは限らないため, `toString` の型は矛盾する
-- これはランク2のケースだが, ランクがいくつでも考え方は同じである.
-- つまり, `forall` が冠頭に来た関数の適用時に型変数は具体化する


-- ==========================================
-- ExistentialQuantification による forall
-- ==========================================
--
-- 言語拡張 `ExistentialQuantification` を指定すると,
-- 型定義の右側だけに現れる型変数を使うことが可能.
--
-- data ShowType = forall a. Show a => ST a
--
-- これも考え方は同じ.
-- つまり, 値コンストラクタ `ST` を呼び出す際に型変数 `a` が決定される.
-- そして型の上に `a` はあらあれない. そういう型である.
-- 
