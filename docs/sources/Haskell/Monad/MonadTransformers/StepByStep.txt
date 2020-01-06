=================================
Monad Transformer Step By Step
=================================

これは, モナドトランスフォーマーステップバイステップ_ を元とした記事である. この引用元の記事も, 「Monad Transformer Step by Step (Martin Grabmuller)氏の記事を翻訳したものである.

.. _モナドトランスフォーマーステップバイステップ: http://bicycle1885.hatenablog.com/entry/2012/12/08/165236

0. Abstract
==============

このチュートリアルは, Haskellのプログラムに徐々に機能を追加していくためにどのようにモナドトランスフォーマーを利用したらよいかを説明する.

これはモナドトランスフォーマー自体の「実装」に関するものではない. エレガントでスッキリしていて, かつ強力なHaskellプログラムを書くために, いかにモナドトランスフォーマーを「利用」するかについてのチュートリアルである.

単純な式を評価する関数から始め, モナドスタイルに変えてから, モナドトランスフォーマーを構築しつつ, エラー処理, 環境渡し, 状態, ログ, 入出力といった機能を加えていく.

1. Introduction
================

モナドはプログラムを組み上げていく上で, 柔軟で拡張性を持つ非常にエレガントな手法である. モナドはHaskellのような遅延評価関数型言語ではとても興味深いもので, 純粋関数型のプログラムに副作用を統合させる事ができる. さらに, モナドを使ってプログラムを構築していくことで, ちょっとした定義があれば, 色々なアルゴリズムの必要な記録処理や裏側の処理の多くを隠すことができ, 注目しているアルゴリズムに集中することができる.

モナドトランスフォーマーはモナドを使ったプログラミングをさらに便利にしてくれる. 違ったモナドや型やモナドを連結する関数のライブラリを提供してくれるので, 必要なモナドトランスフォーマを組み合わせて特性のモナドを作ることができる. 例えば, 状態とエラー処理を備えたモナドが欲しいなら, `StateT` と `ErrorT` 二つのモナドトランスフォーマーを選んできて組み合わせれば良い. このチュートリアルの目的は, シンプルな関数から始め, それをステップバイステップで機能を拡張すべく色々とモナドを操って広げていく様を優しく紹介することである. このチュートリアルはモナドトランスフォーマーの概念の裏に潜んだ理論についてものではないし, 実装についてのものでもない. 

ここでは, Haskellのモナドクラスとかdo記法などといった, モナドを使ったプログラミングの機能や基礎については知っていることを想定している. 他のことは途中で追々説明していく. 

ここにある, Haskellのプログラムは現在のHaskell98標準のものではない機能を使っている. `Control.Monad.Error` などは標準ライブラリのモジュールではない. こうしたモジュールにある階層的なモジュール名も細かい実装も, Haskell98の範疇ではない. しかしながら, こうした拡張は現在のGHCではちゃんとサポートされている. プログラムはGHCのバージョン7.4.1で確認している.

モナドトランスフォーマーのモジュールは, Mark P.Jonesの論文にインスパイアされている. その中には, とても読みやすいMonadプログラミングのイントロダクションはあるが, このチュートリアルほどは実践的ではない.

コンピュータを前にしてこのチュートリアルを読むのがベストだろう. Haskellの標準ライブラリやモナドトランスフォーマのライブラリにある, いろいろな関数の説明を調べたり, 型を調べたりできるようにである. このチュートリアルを印刷して, ウェブブラウザでオンラインのライブラリのドキュメントを開いて, Transformerモジュールをロードしてghciで対話的に, `:type` で型を確認しながら実行してみるのが一番いいだろう.

1.1 プログラム例 (Example Program)
-------------------------------------

実行する例として, シンプルなプログラミング言語のインタープリタをこのチュートリアルを通して使う. すべてのコードはTransformersというモジュールに収められ, 次のようなコードが頭にある.

.. code-block:: hs

    module Transformers where

    import Control.Monad.Identity
    import Control.Monad.Error
    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Writer
    import Data.Maybe
    import qualified Data.Map as Map

Control.Monadで始まるインポートされたモジュールは, その中で定義されたモナドトランスフォーマーを使うときだけ必要となる. `Data.Maybe` モジュールは `Maybe a` 型の任意の値をも扱うのに便利な関数を定義していて, `Data.Map` モジュールは有限のマップを定義する. これらは, 環境(変数-値のマッピング)を小さなインタープリタの中で定義するのに使う.

次のデータ型がその言語の中でプログラムをモデリングするのに使われる.

.. code-block:: hs

    type Name = String     --  variable names (変数名)
    data Exp = Lit Integer --  expression (式)
             | Var Name
             | Plus Exp Exp
             | Abs Name Exp
             | App Exp Exp
            deriving (Show)
    data Value = IntVal Integer -- values (値)
               | FunVal Env Name Exp
               deriving (Show)
    type Env = Map.Map Name Value -- mapping from names to value

`Name` 型は標準のString型のただの別名. 普遍的な文字列でなく, 変数名についての話であることを明確にするときに使われる. `Exp` データ型は, 整数リテラル・変数・加算・λ式(抽象)・関数適用へのヴァリアントを持っている. 評価されるプログラムは `Exp` データ型からできていて, 結果は `Value` 型である. `Value` は整数か関数(クロージャ)である. `FunVal` の構成要素である `Env` は, λ抽象が評価される環境である.

モナドトランスフォーマを用いる具体例は, 上に示した小さな言語のインタープリタなので, まずは評価関数をつくることから始める. この関数はモナドを使っておらず, ある種の「参照実装」となっている. インタープリタ関数 `eval0` は単純である.

さて, 評価関数を示す前に少しだけ前提の確認をしておく. 上述しているが, 評価されるのは `Exp` 型であり, 評価結果は `Value` 型として表される. `Exp` 型は, 整数リテラル (`Lit Integer`), 変数 (`Var Name`), 加算 (`Plus Exp Exp`), λ式(抽象) (`Abs Name Exp`: \variable -> expression), 関数適用 (`App Exp Exp`) を表現しており, 評価関数 `eval0` はこれらそれぞれを評価する関数である. 始めはモナドなしということで, パターンマッチを駆使して各表現を評価する関数を記述することになる. 評価後, 最終的な値は `Value` 型で表現される値になる. すなわち, 数値(`IntVal`)もしくは関数(`FunVal`)である. 
また, `Env` 関数はいまいちよくわからないが, 変数名から値へのマッピングを行う際の環境(λ抽象が評価される環境)なので, 変数名から値を取り出す際には使いそうだなという雰囲気だけ分かればいいだろう.

さて, 順に評価関数の内容を順に考えてみる. 整数リテラル (`Lit Integer`) は単純な整数なのでそのまま `Value` の `IntVal` に値をくるんであげればいい. 次に 変数だが, これは評価時に値との関連付けがひつようなので, 環境 `Env` が必要になる. つまり, `Env` から関連付けられている式を引き出して評価するという形になる. 加算 `Plus` は単純に2つの式を評価しその和を返す. ラムダ式は最終的に式 `FunVal` になるが, 評価される環境を捉える必要がある. 関数適用も適用した後に式 `FunVal` になるが, 最初に関数と引数を評価し, 加算と同じように処理される.

.. code-block:: hs

    eval0 :: Env -> Exp -> Value
    eval0 env (Lit i) = IntVal i
    eval0 env (Var n) = fromJust (Map.lookup n env)
    eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                                 IntVal i2 = eval0 env e2
                             in IntVal (i1 + i2)
    eval0 env (Abs n e) = FunVal env n e
    eval0 env (App e1 e2) = let val1 = eval0 env e1
                                val2 = eval0 env e2
                            in case val1 of
                              FunVal env' n body -> eval (Map.insert n val2 env') body

もちろん上記の評価関数には粗がある. 例えば, 変数の評価では, どこにもラムダ式で束縛されていない変数が使われるとエラーとともにプログラムが停止する(fromJust を Nothing で評価することになるため). また, 加算の評価では, どちらの式も最終評価値が `IntVal` でないとパターンマッチに失敗する. また, 関数適用の評価におけるcase式は, また別のエラーを吐く可能性もあるだろう.

この説明だけでは頭の悪い私にはよくわからないので, 少し追加で説明を加えよう. `Env` 型は, `Map.Map Name Value` のシノニムになっているが, `Map.Map` 型はそもそもなんだろう. 調べてみるとこれは, `containers` というパッケージに以下のように定義されていた.

.. code-block:: hs

    data Map k a

説明には「キー"k"と, 値"a"を持つMap型」と書いてある. なるほどこれはPythonにおける辞書であると考えればいいのだろう. そうなると, `Map.insert`, `Map.lookup` 関数もなんとなく関数名で動作を理解できる. 一応ちゃんと調べてみると, それぞれ以下のようであった.

.. code-block:: hs

    -- 挿入関数
    -- すでにkeyが存在していると, a (value) の値を書き換える.
    -- keyが存在していないなら, (k, a) のペアを追加する.
    insert :: Ord k => k -> a -> Map k a -> Map k a

    -- 検索関数
    -- 検索に使うkeyと検索対象のデータを渡し,
    -- マッチしたら, Justに包んでvalueを返す.
    -- マッチしなかったら, Nothingを返す.
    lookup :: Ord k => k -> Map k a -> Maybe a

つまり, `App` の時はenvに対して挿入し, `Var` ではenvに対して検索をかける. この評価関数の第一引数であるenvは, 下の例でも示すが, `Map.empty` を利用する.

.. code-block:: hs

    -- 空Map
    empty :: Map k a

つまり, 初めに空マップを作っておき, インタープリタが処理されていくに連れ, ここに挿入されたり検索されたりするわけである.


さて, このインタープリタを試してみよう.

.. code-block:: hs

    exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
    eval0 Map.empty exampleExp
    > IntVal 18

これに関しては, 丁寧に処理過程を見ていこう.

まずは, `Plus` が処理されるので, `Lit12` と `App (Abs "x" (Var "x")) (Lit 4 ...)` が処理される.

前者は, `IntVal 12` である.

後者は更に展開される. `App (Abs ..) (Lit ..)` の評価時に両方を評価しそれぞれ変数に束縛する. `(Abs "x" (Var "x"))` は `FunVal Map.empty "x" (Var "x")` となり, `(Lit 4 ...)` は `IntVal 6` になる. case文を無事クリアするため, `App (Abs ..) (Lit ..)` は `eval0 (Map.insert "x" (IntVal 6) Map.empty) (Var "x")` となる.

`Map.insert` 関数により, `("x", IntVal 6)` というペアが保存され, 保存されたあとの `Map.Map k a` 型つまり, `Env` 型の値(`env'`)が返される. そのため結果的に, `App (Abs ..) (Lits ..)` は `eval env' (Var "x")` と評価される.

さて, これを評価するとはすなわち, `Map.lookup` で `env'` を `"x"` でマッチングする. これは必ず成功し(入れたものをそのまま取り出すため), `IntVal 6` が帰ってくる.

最後に, はじめの式の評価に戻り, `IntVal 12` と `IntVal 6` の加算になるため, 結果として `IntVal 18` ということになる.


2. モナドトランスフォーマー(Monad Transformers)
====================================================

モナドトランスフォーマーを使う目的は, 状態・エラー・環境といった計算の側面をコントロールすることである. すでにあるプログラムにモナドを使って書き直すのは少々めんどくさいが, 一度やってしまうとモナドが絡む部分を加えたり, 削ったり, 変更したりするのは比較的簡単である.

この節では, 1.1節のプログラムをモナドを使って書き直し, データ型や関数定義を様々なモナドトランスフォーマーの型や関数を使って除々に拡張していく.

2.1 モナドスタイルに (Converting to Monadic Style)
----------------------------------------------------

モナドトランスフォーマーを使うには, 関数をモナドスタイルで表現しなければならない. それはつまり, 逐次処理をdo記法を使ったモナド操作で書き, 関数の結果を示すために, return関数を使う必要があるということである.

まず, 評価装置が定義されるモナドを定義する. 下の `Eval1 a` を `Identity a` 型の別名として定義する. `Identity` は, `Control.Monad.Identity` からインポートされたモナドで, 想像しうる限り一番単純なモナドだろう. `Identity` は, 標準的な `return` や `(>>=)` をモナド操作の構築のために定義していて, `runIdentity` 関数をそのような操作を実行するために追加で定義している. それ以外には `Identity` モナドは何もしない. ある意味で, このモナドを「基礎」として使い, 周りを別のモナドで包むこともできる. 可読性のため, ただ `runIdentity` を呼ぶための `runEval1` も定義している. `runEval1` は `runIdentity` を呼び出すだけである.

.. code-block:: hs

    type Eval1 a = Identity a

    runEval1 :: Eval1 a -> a
    runEval1 ev = runIdentity ev

Eval1モナドをもとに `eval0` 関数を, `eval1` として書き直す.
基本的には型にあるように, 結果が `Eval1` モナドに包まれているようにする.
単純に言えば `return` を使って上げれば良いが, 一部少しひねっている.

.. code-block:: hs

    eval1 :: Env -> Exp -> Eval1 Value
    eval1 env (Lit i) = return $ IntVal i
    eval1 env (Var n) = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
    eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                                IntVal i2 <- eval1 env e2
                                return $ IntVal (i1 + i2)
    eval1 env (Abs n e) = return $ FunVal env n e
    eval1 env (App e1 e2) = do val1 <- eval1 env e1
                               val2 <- eval1 env e2
                               case val1 of
                                 FunVal env' n body ->
                                   eval1 (Map.insert n val2 env') body

上記の実装を見てみると, `Lit` と `Abs` に関しては単純に, `return` 関数を使うだけでいい. `App` に関しては, 中で `eval1` 関数を呼び出すだけで完結する(つまり, `App` の場合は直接値を返さないため, `return` 関数を使わない). `Plus` に関しては注意が必要である. まずモナドの逐次処理なのでdo記法になっていること. そして, `Plus` の引数を評価関数に渡した結果を使って加算するが, その結果は `Eval Value` となっているため `<-` を使うことである. 最終的に返す値に関しても, `return` 関数をつけることを忘れないこと. 最後に, `Var` に関してであるがこれに付いては少し関数を紹介する. `maybe` と `fail` 関数である.

.. code-block:: hs

    -- 基本的に第二引数の関数を, 第三引数に適用させる.
    -- 第三引数がMaybeなので, Just値なら適用後の値が結果になる.
    -- 第三引数がNothingならば, 第一引数(デフォルト値)を返す.
    maybe :: b -> (a -> b) -> Maybe a -> b

    -- メッセージ付きで失敗する関数
    -- モナドの定義関数ではないが, do記法内のパターンマッチの失敗
    -- などで発動する.
    fail :: Monad m => String -> m a

さて, 冷静に実装を見てみると, 第一引数は関数適用した `fail` 関数である. 第二引数は `return` であり, 第三引数は `Map.lookup` で取得した `Maybe` 値である. つまり, `Map.lookup` が成功したら `return` によって `Eval Value` として値が返るが, `Map.lookup` が `Nothing` なら `fail string`, つまりメッセージを出力し失敗する.

.. code-block:: hs

    runEval1 (eval1 Map.empty exampleExp)

とすれば,

.. code-block:: hs

    IntVal 18

となる.
また, 失敗の例を見るために, 以下のような関数を実装し

.. code-block:: hs

    failureExampleExp :: Exp
    failureExampleExp = Lit 12 `Plus` (App (Abs "x" (Var "y")) (Lit 4 `Plus` Lit 2))

以下のように実行すると, 失敗時の挙動を確認できる.

.. code-block:: hs

    runEval1 (eval1 Map.empty failureExampleExp)


要約すれば, `return` 関数を使って関数の結果を返すことと, do記法や (>>=) や (>>) 関数を使ってモナドアクションを逐次実行することの2つからモナドへの変換は成り立っている.

.. note::

    eval1 の型は, 以下のように一般化できる.

    .. code-block:: hs

        eval1 :: Monad m => Env -> Exp -> m Value

    これは, `return` と `do` 記法に隠された (>>=) 以外には, どんなモナド操作を行っていないためである. こうすれば, どんなモナドの文脈でも eval1 を使うことができ,

    .. code-block:: hs

        runEval1 (eval1 Map.empty exampleExp)

    とする代わりに, ghciで, 

    .. code-block:: hs

        eval1 Map.empty exampleExp

    と書ける. これは, IOモナドの内部で式を実行する. インタープリタは内部的に `print` 関数を使っているが, この関数はIOモナド内部でしか利用できないためである. これは嬉しいこともあるが, 特定のモナドに固有の操作を行うことが多いだろうし, 特定のモナドに操作は縛られる.


2.2 エラー処理を加える (Adding Error Handling)
--------------------------------------------------

これまで見てきたように, 現状の評価関数は粗が多い. 例えば束縛されていない変数や型エラーがある式など, 入力によってはエラーメッセージと共に泊まってしまう.

ローカルのモナドトランスフォーマーライブラリにある `ErrorT` モナドトランスフォーマーを使えば, `Eval1` モナドを基にして `Eval2` へ拡張できる.

.. code-block:: hs

    type Eval2 a = ErrorT String Identity a

`ErrorT` のString型引数は例外の型で, エラーの状態を示すために使われる値である. ここでは簡単のためにStringを使うが, 実際の実装ではコンパイラだとソースコードの一だとか, ウェブアプリケーションだとタイムスタンプだとかがあるといいかもしれない.

`Eval2` モナド内での計算を実行する関数は2つ異なる点がある. ひとつ目は評価の結果が `Either String a` 型で, `Left s` だとエラーが置きたことをエラーメッセージ `s` で示し, `Right r` だと評価が成功したことを結果 `r` で表す. ふたつ目は, `runErrorT` 関数を与えられた計算に対して呼び出し, `Identity` の計算が返され, 今度はそれが `runIdentity` を使って評価されるという点である.

.. code-block:: hs

    runEval2 :: Eval2 a -> Either String a
    runEval2 ev = runIdentity (runErrorT ev)

さて, もう `eval1` 関数の型を単純に書き換えるができて, 次のバージョン(eval2a) を実装できる.

.. code-block:: hs

    eval2a :: Env -> Exp -> Eval2 Value
    eval2a env (Lit i) = return $ IntVal i
    eval2a env (Var n) = maybe (fail ("[CatchError] undefined variable: " ++ n)) return $ Map.lookup n env
    eval2a env (Plus e1 e2) = do IntVal i1 <- eval2a env e1
                                 IntVal i2 <- eval2a env e2
                                 return $ IntVal (i1 + i2)
    eval2a env (Abs n e) = return $ FunVal env n e
    eval2a env (App e1 e2) = do val1 <- eval2a env e1
                                val2 <- eval2a env e2
                                case val1 of
                                  FunVal env' n body
                                    -> eval2a (Map.insert n val2 env') body

このバージョンは上で定義された `runEval2` 関数を使って実行できる. この関数をサンプルの式に適用すると, 結果は `Right` コンストラクタに包まれている点のみが変わっている.

.. code-block:: hs

    runEval2 (eval2a Map.empty exampleExp)
    > Right (IntVal 18)

しかし, これではだめで, 正しくない式が与えられると `ErrorT` トランスフォーマーのエラー報告は使われない. もっと有用なエラーメッセージを吐くためには改良が必要である.

.. code-block:: hs

    eval2b :: Env -> Exp -> Eval2 Value
    eval2b env (Lit i) = return $ IntVal i
    eval2b env (Var n) = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
    eval2b env (Plus e1 e2) = do e1' <- eval2b env e1
                                 e2' <- eval2b env e2
                                 case (e1', e2') of
                                   (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                   _ -> throwError "type error"
    eval2b env (Abs n e) = return $ FunVal env n e
    eval2b env (App e1 e2) = do val1 <- eval2b env e1
                                val2 <- eval2b env e2
                                case val1 of
                                  FunVal env' n body -> eval2b (Map.insert n val2 env') body
                                  _ -> throwError "type error"

これで, 正しくない式を評価しようとすると, Leftコンストラクタに包まれたエラーメッセージが出る. そして, 評価関数に対してパターンマッチングすることで, 通常の結果とエラーの結果を区別することができる.

.. code-block:: hs

    runEval2 (eval2b Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))
    > Left "type error"

エラーをキャッチすることができた!

`eval2b` 関数をもうちょっと良く調べ得てみれば, do式の内部のモナド結合ではパターンマッチングが失敗したときに `fail` 関数を呼び出すということをうまく使うと, もっと短く(良く?)できることがわかる. そして, 今まで見てきたように, `fail` 関数は思ったとおりに動いてくれる.

.. code-block:: hs

    eval2c :: Env -> Exp -> Eval2 Value
    eval2c env (Lit i) = return $ IntVal i
    eval2c env (Var n) = maybe (fail("undefined variable: " ++ n)) return $ Map.lookup n env
    eval2c env (Plus e1 e2) = do IntVal i1 <- eval2c env e1
                                 IntVal i2 <- eval2c env e2
                                 return $ IntVal (i1 + i2)
    eval2c env (Abs n e) = return $ FunVal env n e
    eval2c env (App e1 e2) = do FunVal env' n body <- eval2c env e1
                                val2 <- eval2c env e2
                                eval2c (Map.insert n val2 env') body

すこし丁寧に説明しよう. `eval2b` において, `Plus` 型の処理部分では, 評価結果がどちらも `IntVal` であれば加算処理を行い, そうでなければ `throwError` を投げるというコードだった. しかし, パターンマッチに失敗した場合は `fail` 関数が呼ばれるため, 結果の確認処理を省いても, エラーを補足してくれるため動作自体はする. ただし, その場合はエラーが起きた際に投げられるエラーメッセージは "pattern match failure ... " というあまり良いエラーメッセージではないことに気をつける. これが, 上記の例を敢えて「短く」書けると称し, 「良く」書けると称さなかった理由である. もっと良いエラーメッセージがほしいなら, 自分で `throwError` をするほうがいいだろう.

さて長くなったが, 以下がエラー処理評価をする最終バージョンである.

.. code-block:: hs

    eval2 :: Env -> Exp -> Eval2 Value
    eval2 env (Lit i) = return $ IntVal i
    eval2 env (Var n) = case Map.lookup n env of
                          Nothing -> throwError ("unbound variable: " ++ n)
                          Just val -> return val
    eval2 env (Plus e1 e2) = do e1' <- eval2 env e1
                                e2' <- eval2 env e2
                                case (e1', e1') of
                                  (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                  _ -> throwError "type error in addtion"
    eval2 env (Abs n e) = return $ FunVal env n e
    eval2 env (App e1 e2) = do val1 <- eval2 env e1
                               val2 <- eval2 env e2
                               case val1 of
                                 FunVal env' n body -> eval2 (Map.insert n val2 env') body
                                 _ -> throwError "type error in application"

.. note::

    `Control.Monad.Error` モジュールは `throwError` で生じたエラーを捕まえるように, 別の関数も提供している. それが, `catchError :: m a -> (e -> m a) -> m a` で, 任意のエラーモナドに使える. 局所的なエラー処理にもエラーを上位に渡すことにも使える.

2.3 環境を隠す (Hiding the Environment)
-------------------------------------------

評価関数の定義をもっといいものにする方法の一つは, すべての関数定義と呼び出しから環境を隠すことである. 環境が展開されている場所は一箇所(関数適用)だけ, 実際に使われる場所は二箇所 (変数とラムダ式) だけなので, 他の場所では環境を隠せばコードの量を減らすことができる. これはリーダーモナドを実装するために `ReaderT` モナドトランスフォーマーを加えれば表現できる. `Reader` モナドは, 値をその下のすべての計算に渡す. この値は内側の計算から読むことができ, 入れ子になった計算の中では改変することもできる. 状態(`State`)モナドと異なり, カプセル化された計算は上位の計算で使われた値を改変することができない.

以前のモナドを単に `ReaderT` コンストラクタで包むことから始める.

.. code-block:: hs

    type Eval3 a = ReaderT Env (ErrorT String Identity) a

実行関数 `runEval3` は初期環境を与える必要があるので少し変更する必要がある. 評価関数から環境引数を削ることが目的である.

.. code-block:: hs

    runEval3 :: Env -> Eval3 a -> Either String a
    runEval3 env ev = runIdentity (runErrorT (runReaderT ev env))

    eval3 :: Exp -> Eval3 Value
    eval3 (Lit i) = return $ IntVal i
    eval3 (Var n) = do env <- ask
                      case Map.lookup n env of
                        Nothing -> throwError ("unbound variable: " ++ n)
                        Just val -> return val
    eval3 (Plus e1 e2) = do e1' <- eval3 e1
                            e2' <- eval3 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                              _ -> throwError "type error in addition"
    eval3 (Abs n e) = do env <- ask
                         return $ FunVal env n e
    eval3 (App e1 e2) = do val1 <- eval3 e1
                           val2 <- eval3 e2
                           case val1 of
                             FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
                             _ -> throwError "type error in application"

`Reader` モナドで利用する補助関数 `ask`, `local` の説明をしておく.

.. code-block:: hs

    -- 環境を回収する
    ask :: m r

    -- 環境を変更する計算(関数)を実行する
    local :: (r -> r) -> m a -> m a

`ask` 関数に関してはいいだろう. 問題は `local` 関数が使われている部分である. `local` の型定義から `(r -> r)` の部分が `const (Map.insert n val2 env')` であり, `m a` の部分が `eval3 body` である. ここで, `const (Map.insert n val2 env)` の意味を考えてみる. 本来 `local` に渡すのは「状態を更新するための関数」であるが, 今は `Map.insert n val2 env'` によって更新された状態が取得できているような状況であり, いわばこの状態と以前の状態を入れ替えたいのである. ここで, `const` 関数を利用する. これは, `const :: a -> b -> a` という型の関数で, 「第一引数と第二引数を受け取り, 結果として第一引数を返す」という関数である. すなわち, この関数に新しい状態を適用させた関数 `const (Map.insert n val2 env)` を `local` 関数で利用してあげると, 「現在の状況を `const` の第二引数にとる」ような形にすることができ, 結果として第一引数として適用した状態が返されるため, 状態を入れ替えることができるのである. Readerモナドを利用することで, これまでのような引数の流れではなく, `Env` が外に出ていることに注意する.

サンプルを実行するなら, 以下のように評価する.

.. code-block:: hs

    runEval3 Map.empty (eval3 exampleExp)

モナドが複雑になってきたので, ここで一度この関数の動作の流れを順にとらえてみる. この関数は以下の関数と同値である(exampleExpを展開しただけ)

.. code-block:: hs

    runEval3 Map.empty (eval3 (Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))))

`runEval3 :: Env -> Eval3 a -> Either String a` であり, `eval3 :: Exp -> Eval Value` であり, `exampleExp :: Exp` であった. さて, `eval3` 関数の処理からはじまり, `Plus` の処理を行う. `Lit 12` は `Eval3 IntVal 12` となり, `App (Abs "x" (Var "x")) (Lit 4 `Plus\` Lit 2)` は更に評価が進むことに成る. `App` の処理に入るため, `Abs "x" (Var "x")` と `(Lit 4 `Plus\` Lit 2)` を評価することになり, 前者は `Eval3 FunVal Map.empty "x" (Var "x")` となるため, `val1 = FunVal Map.empty "x" (Var "x")` であり, 後者は `Eval3 IntVal 12` となるため, `val2 = IntVal 12` である. これは, case文で制御され, `Map.insert "x" (IntVal 12) Map.empty` で得られる新しい環境 `(env'')` を第一引数として適用した `const` 関数 `const (Map.insert n val2 env')` と, `(Var "x")` を評価する `eval3 body` を第二引数として, `local` 関数が呼ばれる. この `local` 関数では, 第二引数における環境として, 第一引数の環境が利用されるため, `Map.empty` から一度更新された環境 `env'` を利用する. そのため, `Var` の処理における `Map.lookup` で値が発見され, 期待した動作結果をえる.

.. note::

    `ask` に加え, `asks` 関数が予め定義されていて環境から値へとマッピングする関数を取る. これはレコードセレクター関数(record selector function)に `ask` を適用して環境のそれぞれの構成要素を取り出すのに使うこともできる.

    レコードセレクター関数とは, データ型を record syntax で定義した際に, コンパイラが自動的につくるフィールドの値を取得する関数のことである.

2.4 状態を加える (Adding State)
-----------------------------------

重要なモナドの応用例として, 純粋関数型のプログラムに変更可能 (mutable) な状態を可能にすることが挙げられる. これは, Stateモナドを用いれば良く, 初期状態の設定・現在の状態の問い合わせ・変更といった操作を提供してくれる.

例として, このインタープリタにプロファイリング機能を付け足したいとする. 最も内側のモナド (`Identity`) を `StateT` コンストラクタで包んで新しいモナドを定義する. (`State` モナドと `Error` モナドに関しては, 後でみるようにコンストラクタの順番が問題になる). 例では, 単純な整数値を状態として保持するが, どんなデータ型の値でも可能である. 通常は, 目下の仕事に必要十分な状態を記録として保持することになる.

.. code-block:: hs

    type Eval4 a = ReaderT Env (ErrorT String (StateT Integer Identity)) a

`runEval4` 関数の返り値の型は変わる. 最終的な状態が評価結果(エラーか計算値)と一緒に返されるからである. さらに, 初期状態を追加の引数(`Integer`)で渡し(これが `State`), 例えば別の計算の最後の状態から再度計算を始めることができるように, 柔軟性をもたせている.

.. code-block:: hs

    runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
    runEval4 env st ev = runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

単純な例として, 評価のステップ数を数えるだけにしよう. つまり `eval4` 関数を呼んだ回数である. 状態の変更は, すべて小さな補助関数 `tick` の中で起き, 隠された状態を計算から取りだし, カウンタを増やして, 状態を戻す. この先の節で再利用するつもりなので, もちろん `tick` の型は `Eval4 ()` ではない. そのため, 中で `tick` が使われるモナドは状態モナド(この場合, `MonadState` 型クラスのこと)で, そのモナドの中で操作される状態は(+)演算子を使えるように数値にするというに留める.

.. code-block:: hs

    tick :: (Num s, MonadState s m) => m ()
    tick = do st <- get
              put (st + 1)

それぞれの場合で, `tick` 関数の呼び出しを加えれば, 適用の回数を数えることができる.

.. code-block:: hs

    eval4 :: Exp -> Eval4 Value
    eval4 (Lit i) = do tick
                       return $ IntVal i
    eval4 (Var n) = do tick
                       env <- ask
                       case Map.lookup n env of
                         Nothing -> throwError ("unbound variable: " ++ n)
                         Just val -> return val
    eval4 (Plus e1 e2) = do tick
                            e1' <- eval4 e1
                            e2' <- eval4 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                              _ -> throwError "type error in additional"
    eval4 (Abs n e) = do tick
                         env <- ask
                         return $ FunVal env n e
    eval4 (App e1 e2) = do tick
                           val1 <- eval4 e1
                           val2 <- eval4 e2
                           case val1 of
                             FunVal env' n body -> local (const (Map.insert n val2 env')) (eval4 body)
                             _ -> throwError "type error in application"

以下のようにサンプルを評価できる

.. code-block:: hs

    runEval4 Map.empty 0 (eval4 exampleExp)
    > (Right (IntVal 18), 8)

これは評価が成功して, 整数18を返し, 簡約が8ステップだったことを表している.

.. note::

    `Eval4` モナドの型を次のように変える. (`StateT` と `ErrorT` を入れ替えた)と, モナドの解釈がく変わる.

    .. code-block:: hs

        type Eval4' a = ReaderT Env (StateT Integer (ErrorT String Identity)) a

    結果 (エラーか否か) と状態を返す代わりに, エラーか結果と最終的な状態かを返す. これは対応する実行関数の型からもわかるだろう.

    .. code-block:: hs

        runEval4' :: Env -> Integer -> Eval4' a -> (Either String (a, Integer))
        runEval4' env st ev = runIdentity (runErrorT (runStateT (runReaderT ev env) st))

    最終的な状態には関与しないので, `Reader` モナド変換子の位置は問題にはならない.

.. note::

    `State` モナドは `gets` 関数も提供しており, 結果を返す前に状態に対して関数を適用する. 内部状態に関数を適用することによって状態を変更する `modify` 関数もある.


2.5 ログを加える (Adding Logging)
-----------------------------------

ここで述べる最後のモナドトランスフォーマーは `WriterT` である. ある意味 `ReaderT` の対になるものである. `WriterT` が提供する関数は, 渡された値を用いるものではなく, 計算結果に値を追加するものだからである.

.. code-block:: hs

    type Eval5 a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Identity))) a

`StateT` の場合と同様に, 結果を出力するものなので `WriterT` は `ErrorT` と干渉する. `ErrorT` と `WriterT` の順番によって, エラーが起きたときに, `Writer` モナドの結果が値を返すかどうかが変わる. 書き出される値, は文字列のリストである. `WriterT` モナドトランスフォーマーのドキュメントを見ると, 書き出された値の型はモノイド (Monoid) 型クラスのメンバーに制限されることがわかるだろう. このことは, このクラスのメソッドは内部的に初期値を構成したり, 書き出される値を合成したりするのに使われるため必要なことである.

実行関数は先程と同様に拡張される.

.. code-block:: hs

    runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
    runEval5 env st ev = runIdentity (runStateT (runWriterT (runErrorT (runReaderT ev env))) st)

評価関数では, 評価中に遭遇した変数名を書き出すことによって, Writerモナドの使い方を説明する.

.. code-block:: hs

    eval5 :: Exp -> Eval5 Value
    eval5 (Lit i) = do tick
                       return $ IntVal i
    eval5 (Var n) = do tick
                       tell [n]
                       env <- ask
                       case Map.lookup n env of
                         Nothing -> throwError ("unbound variable: " ++ n)
                         Just val -> return val
    eval5 (Plus e1 e2) = do tick
                            e1' <- eval5 e1
                            e2' <- eval5 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                              _ -> throwError "type error in additin"
    eval5 (Abs n e) = do tick
                         env <- ask
                         return $ FunVal env n e
    eval5 (App e1 e2) = do tick
                           val1 <- eval5 e1
                           val2 <- eval5 e2
                           case val1 of
                             FunVal env' n body -> local (const (Map.insert n val2 env')) (eval5 body)
                             _ -> throwError "type error in application"


以下のようにサンプルを評価できる

.. code-block:: hs

    runEval5 Map.empty 0 (eval5 exampleExp)
    > ((Right (IntVal 18), ["x"]), 8)

これは評価が成功して, 整数18を返し, 簡約が8ステップだったことに加え, 遭遇した変数名について取得できていることがわかる.


2.6 IOはどうすんのさ? (What about I/O?)
------------------------------------------

これまでのところ, ある重要な面を考慮していない. すなわち, 入力と出力である. どのようにして, これまで開発してきたモナドの定義に, 入出力を導入すればいいだろうか. IOモナドトランスフォーマーを定義することはできない. なぜなら, HaskellのIO操作の実行は, 他の関数やモナドに勝手に入れ子にしてはならず, IOモナドでのみ可能になっているからである. 幸運なことに, モナドトランスフォーマーのライブラリは, 我々が組み上げてきたものに簡単にIO操作を導入する下地を提供してくれている. 単にIdentityの代わりにIOを使えばいい! これは, Identityが基になるモナドで, 今まで見てきたように, このモナドのアクションを評価する `runIdentity` 関数は, いつでも一番最後に適用されていたからである.

.. code-block:: hs

    type Eval6 a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO))) a

`runEval6` が返す型は, IO構築子によって包まれている. `Eval6` の計算を実行することは直接的に結果を出すのではなく, 結果を得るには実行しなければならないIO計算を返す.

.. code-block:: hs

    runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
    runEval6 env st ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) st

`eval6` 関数では, ちょっとした一手間でIO操作を行うことができる. それには `liftIO` 関数を使って操作を呼び出す必要があり, これでIO計算を現在実行中のモナドに持ち上げることができる. 例として, 整数定数が評価される度にプリントすることにする. (こればいい方法だとは思わないが, 要点を説明でき, 良いデバッグのテクニックになることもある)

.. code-block:: hs

    eval6 :: Exp -> Eval6 Value
    eval6 (Lit i) = do tick
                       liftIO $ print i
                       return $ IntVal i
    eval6 (Var n) = do tick
                       tell [n]
                       env <- ask
                       case Map.lookup n env of
                         Nothing -> throwError ("unbound variable: " ++ n)
                         Just val -> return val
    eval6 (Plus e1 e2) = do tick
                            e1' <- eval6 e1
                            e2' <- eval6 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                              _ -> throwError "type error in addition"
    eval6 (Abs n e) = do tick
                         env <- ask
                         return $ FunVal env n e
    eval6 (App e1 e2) = do tick
                           val1 <- eval6 e1
                           val2 <- eval6 e2
                           case val1 of
                             FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
                             _ -> throwError "type error in application"

以下のようにサンプルを評価できる

.. code-block:: hs

    runEval6 Map.empty 0 (eval6 exampleExp)
    > 12
    > 4
    > 2
    > ((Right (IntVal 18), ["x"]), 8)

これで先程の例に加え, 整数定数が評価される度に値がプリントされていることがわかる.

3. 結論 (Conclusion)
======================

モナドトランスフォーマーは関数型プログラマーの持てる強力なツールである. このチュートリアルでは, 現在のHaskellの実装で利用できるモナドトランスフォーマーと, 簡単なインタープリタを作るのにそれらをどうやって組み合わせるかを紹介した.

ここでは, 現在実装されているモナドトランスフォーマーを全てカバーしていない. (例えば, 継続やリストのモナドトランスフォーマー). もっと情報を得るために, Haskellのウェブサイトにあるライブラリのドキュメントを読むことをおすすめする.

モナドトランスフォーマーを使うことで, 今のアプリケーションでやりたいことを, 一枚岩でお手製のモナドにまとめ上げ, 様々なアプリケーションに特化したモナドをいとも簡単に作り出すことができるのである.
