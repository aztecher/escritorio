===========
Usage
===========

`directory-tree` パッケージの基本的な使用方法に関して記述していく.

このディレクトリでは, ファイルシステムのディレクトリツリーをミラーするような簡単なデータ構造を定義しており, その構造を制御するような関数などが定義されている.

本パッケージは後学のために少しなぶるつもりなので, 細かい実装の内容やその説明なども別ファイルに記述していくつもりである.

本ページは単純な利用方法にフォーカスして記述していくつもりである.

データ構造
============

本パッケージに定義されているデータ構造のうち, 重要なのは `DirTree` である. `AnchoredDirTree` というデータ構造もあるが, これは `DirTree` にベースディレクトリの情報を与えただけのごく単純なラッパーである.

`DirTree` は, IOエラーをハンドルする機構も備えている. また, `Functor`, `Foldable`, `Traversable`, `Eq`, `Ord`, `Show` のインスタンスになっている.

また, `FilePath` という名前の型もあるが, これは `String` のシノニムである.

ディレクトリをデータ構造にマッピングする
==========================================

指定したファイルパス以下のディレクトリをデータ構造にマッピングして変数に束縛するような関数が用意されている.

.. code-block:: hs

    ghci> tree <- build "../"
    ghci> tree
    ...

`build` 関数は低レベル関数の位置づけにあるため意味もなく使わないほうがいいかもしれない.

.. code-block:: hs

    ghci> tree <- readDirectoryWith return "../"
    ghci> tree
    ...

`readDirectoryWith` を利用しても同じ結果が得られ, こちらは高レベル関数に位置しているので, 素直に使うだけならこちらの関数を利用すればいいだろう. 第一引数には `return` を噛ませているが, これは `readDirectoryWith` 関数の定義が以下のようになっていることに起因する.

.. code-block:: hs

    ghci> :t readDirectoryWith
    readDirectoryWith
      :: (FilePath -> IO a) -> FilePath -> IO (AnchoredDirTree a)

つまり, この `return` 関数は, `FilePath` から `IO FilePath` への持ち上げを行っているわけである. さて, ここで少しこの関数を変更してみよう.

.. code-block:: hs

    ghci> readDirectoryWith (\filepath -> putStrLn . show $ filepath) "../"
    "../XXX"
    "../YYY"
    "../ZZZ"
    ...

 上の例では, 第一引数に `FilePath -> IO ()` という関数を与え, 結果としてこの関数に引数として代入される文字列を表示するようにしてみた. こうすると, 対象となったファイルのフルパスが表示されていることがわかる.

このように, `readDirectryWith` 関数では処理を一つ挟むことができるようである.

また, この関数を遅延評価にするために, `readDirectoryWithL` という関数も提供されている. ただこれに関しては使用方法は同じなため, 上の説明で十分であろう.

また, `AnchoredDirTree` の型コンストラクタは `(:/)` という特殊な形になっている. 面白いのはこれで, `AnchoredDirTree` のベースディレクトリ情報部分と, `DirTree` 部分を分離して(パターンマッチして)変数に束縛できる.

.. code-block:: hs

    ghci> anchor:/dtree <- readDirectoryWith return "./"
    ghci> anchor
    ""
    ghci> dtree
    ...

    ghci> anchor:/dtree <- readDirectoryWith return "./../"
    ghci> anchor
    "."
    ghci> dtree
    ...


データを書き込む
=================
