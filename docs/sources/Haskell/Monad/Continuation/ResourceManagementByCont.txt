================================
継続モナドによるリソース管理
================================

継続モナド...これ何に使うの? という問題に対する一つの回答.

リソース管理の問題
===================

リソース(ファイルのハンドル/ソケット/排他制御ロック/グラフィックのハンドル)管理の問題は, プログラミングを行っていると必ずまとわりつく問題である.

言わずもがな, リソース確保後は然るべきタイミングで確実に解放するひつようがあるが, 実際は往々にしてリソースの解放漏れが発生する. 単に解放するコードを書き忘れるというのが一番単純だろう.

次のようなコードを考えてみる.

.. code-block:: hs

    copyFile :: FilePath -> FilePath -> IO ()
    copyFile from to = do
      h1 <- openFile from ReadMode
      h2 <- openFile from WriteMode
      contents <- hGetContent h1
      hPutStr h2 content
      hClose h1
      hClose h2

これはファイルをコピーする単純な関数であるが, 思いつく限り以下のようなリソース周りのバグが考えられる.

* コピー先のファイルのopenに失敗したら (パーミションがない, ファイルを作ろうとする場所のフォルダが存在しないなど), コピー元のファイルのハンドルが開放されない.
* コピー元のファイルの読み込みに失敗したら (ファイルが破損している, 無効なUTF8文字列を呼んだ際の例外) 二つのハンドルが解放されない.
* コピー先のファイルへの書き込みに失敗したら (ディスクの容量が足りない場合など) 二つ目のハンドルが解放されない
* (あまりないはずだが) １つ目のファイルのハンドル解放に失敗したら, 二つ目のファイルのハンドルが解放されない.

このような類のバグはとても典型的で, 長い年月人々が戦ってきた問題のため, 様々な種類の解決策が考えられてきた.

多くの言語が備える, `try-catch-finally` や, C#の `using`, Goの `defer` ....まだまだ沢山のよく知られていない方法がある.

bracket 関数
==============

Haskellでは, `bracket` というプリミティブがこの用途で用いられる.

.. code-block:: hs

    bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

bracketは３つの引数を取る.
第一引数はリソース `a` を確保する関数, 第二引数がそのリソース `a` を解放する関数で, 第三引数が確保したリソース `a` を用いて行いたい処理の関数になる.

例えば, ファイルへのアクセスを行う処理を `bracket` を用いて書いてみると,
以下のようになる.

.. code-block:: hs

    fileLength :: FilePath => IO Int
    fileLength path =
      bracket (openFile path ReadMode) hClose $ \h -> do
        content <- hGetContents h
        return $! length content

ここではファイルを扱うので,
リソース確保関数 : `openFile path ReadMode` を
リソース解放関数 : `hClose`
を渡している.
また, もう少し細かく指摘すると, `hGetContents` は遅延IOを行ってしまうので, `length` を取った値を返す際に `$!` を使っている.

意図的に例外を発生させてみると, `bracket` が正しく動作することがわかる.

.. code-block:: hs

    invalidAccess :: FilePath -> IO ()
    invalidAccess path =
      bracket (openFile path ReadMode) (\h -> putStrLn "close" >> hClose h) $ \h -> hPutStrLn h "('._.`)"

`ReadMode` で開いたファイルに, `hPutStrLn` で書き込みを行おうとしているので, ここで例外が発生するはずである. また, ファイルが閉じられることをカクニンするために, ファイルを閉じる前にメッセージを表示する.


with系関数
=============

リソースの確保と解放は, 通常は対になっているものである.
故に, リソース確保関数を呼ぶと, 対応する解放関数を呼び出す必要がある.
例えば, ファイルを開いたらファイルを閉じる関数, メモリを確保したらメモリを解放する関数, ソケットを開いたら, ソケットを解放する関数, である.

`bracket` と合わせて使うことを考えると, あるリソース確保関数が普通決まっているとうことである.
そこで, Haskellは予め対となるリソース確保関数と解放関数を, `bracket` に部分適用したユーティリティ関数が用意されている.

例えば, ファイルを扱うものに `withFile` 関数がある.

.. code-block:: hs

    withFile :: FilePath -> IO Mode -> (Handle -> IO r) -> IO r

第一引数と, 第二引数はもともと, `openFile` に渡していた引数である. 第三引数は `bracket` に渡していた, リソースを利用する関数の型と同じ.

つまり, 第三引数以降を考えると, bracketに `openFile` と `hClose` を予め部分適用した型になっている.

そして, 実際に `withFile` はそのように定義されている.

.. code-block:: hs

    withFile :: FilePath -> IOMode -> (Handle -> IO r) IO r
    withFile name mode = bracket (openFile name mode) hClose

同様に, baseパッケージだけでも次のような関数がある.

.. code-block:: hs

    withArray :: Storable a => [a] -> (Ptr a -> IO r) -> IO r
    withBuffer :: Buffer e -> (Ptr e -> IO r) -> IO r
    withCAString :: String -> (CString -> IO r) -> IO r
    withForeignPtr :: ForeignPtr a -> (Ptr a -> IO r) -> IO r
    withMVar :: MVar a -> (a -> IO r) IO r
    withPooll :: (Pool -> IO r) -> IO r

また, リソース確保関数と解放関数を別々に提供する代わりに, with系関数のみを提供するケースも有る. これはライブラリのユーザがリソースの解放を怠ることを絶対に許さない意図であろう.

さて, withFileを用いて最初の例を書き直してみる.

.. code-block:: hs

    copyFile :: FilePath -> FilePath -> IO ()
    copyFile from to = do
      withFile from ReadMode $ \h1 ->
        withFile to WriteMode $ \h2 ->
          content <- hGetContents h1
          hPutStr h2 content

書き直しは機械的にできる. openFile部分をwithFileに書き換え, ファイルハンドルをクロージャで受け取るようにするだけである. その他に考えることは何もない. これで最初に挙げた色々なバグは全てなくなった.

withの問題点
===============

しかし, これでめでたしとなるほど世の中甘くはない.

with系のAPIは極めて厄介な点もあり, 身も蓋もなく言えばこれを使ったコードは書きにくい. (JavaScriptのコールバックで苦労した経験のある人はイメージ社水かもしれない. コールバックは必要に応じてどんどんネストが深くなる)

さて, さきほどの例を少し拡張して, 二つのファイルを連結して別のファイルに書き出すような関数を書いてみる. 普通に書けば次のようになる.

.. code-block:: hs

    catFiles :: FilePath -> FilePath -> FilePath -> IO ()
    catFiles from1 from2 to = do
      withFile from1 ReadMode $ \h1 ->
        withFile from2 ReadMode $ \h2 ->
          withFile to WriteMode $ \h3 -> do
            hPutStr h3 =<< hGetContents h1
            hPutStr h3 =<< hGetContents h2

単純にwithFileが一つ増え, それに伴いネストが深くなる.
見栄え的にも, コード的にも, ネストの変動は歓迎さず, リファクタリング場面での抽象化のしずらさなどの問題を引き起こす要因になる.

とはいえこれらの審美的な問題であればまぁ割り切ってもいいっちゃいいが, この「組み合わせのしずらさ, 抽象化のしずらさ」は本質的な問題をはらんでいる.

例えば, 二つのファイルを連結する代わりに, 任意個のファイル名のリストを受け取り, それらの中身を連結して書き出すような関数を書こうとする場合, どうすればいいだろうか. 頭の体操と思って考えて欲しい.

.. code-block:: hs

    catFiles :: [FilePath] -> FilePath -> IO ()
    catFiles from to = do
      ???

二個, 三個といった定数個と, 任意個の間にはかなり大きな差がある. というのも今回の場合, `withFile` がリソース受け取りのためにネストしたクロージャを要求するため, これを任意個に対応させるには, 普通に考えれば再帰が必要になる.

なので問題を切り分けて, `withFiles` というwithFileの複数ファイルのバージョンがあることにすれば, `catFiles` は次のように書けるはずである.

.. code-block:: hs

    catFiles :: [FilePath] -> FilePath -> IO ()
    catFiles froms to =
      withFile froms ReadMode $ \fromhs ->
        withFile to WriteMode $ \toh ->
          forM_ fromhs $ \fromh -> do
            content <- hGetContents fromh
            hPutStr toh content

然る後に, `withFiles` を何とかして定義する.

.. code-block:: hs

    withFiles :: [FilePath] -> IOMode -> ([Handle] -> IO a) -> IO a
    withFiles [] _mode k = k []
    withFiles (x:xs) mode k =
      withFile x mode $ \h ->
        withFiles xs mode (\hs -> k $ h:hs)

`withFiles` 自体もwith系のAPIのように定義するのがキモで, そこが少しトリッキーだが, そこを決めればあとは自然と埋まるだろう.

しかしここで重要なのは, 実装のややこしさではなく, with系関数の組み合わせが上手く抽象化できないということである. 今回の場合, リストに対する `withFile` のコードが必要になった. では, この二つのwith系関数を組み合わせて, リソースのタプルに対するwith関数を作るにはどうすればいいだろうか. タプルのためにそれ用の組み合わせ関数を用意スべきだろうか? では, `Maybe` の場合は? `Either` の場合は?

`Foreign.Marshal.Utils` に `WithMany` という関数がある. この関数はwith系関数と, それに適用する値のリスト, それから確保したリソースのリストを受け取る関数を受け取る.

.. code-block:: hs

    withMany :: (a -> ( b -> res ) -> res) -> [a] -> ([b] -> res) -> res

これを使うと, `withFiles` は次のように成る.

.. code-block:: hs

    withFiles fs mode =
      withMany (`withFile` mode) fs

その他に, `Maybe` 関係のいくつかのユーティティ関数が用意されて入るものの, これだけで快適にコードが書けるとは言い難いだろう.

継続渡しスタイル(CPS)
========================

さて, with系の関数は, いわゆる継続渡しスタイルと見ることができる.

継続渡しスタイル(Continuation Passing Style, CPS)と呼ばれるものは, 関数が値を返す代わりに, 返り値を受け取る別の関数を渡すというものである.

.. code-block:: hs

    foo :: Int -> Int
    foo n = n * 2

このような単純な関数があったとして, CPSは以下のようになる

.. code-block:: hs

    fooCont :: Int -> (Int -> a) -> a
    fooCont n k = k (n * 2)

    ghci> fooCont 123 $ \i -> print i
    246

こんなことして何が嬉しいかというと, 実際の所この例で嬉しいことはなにもないが, コンパイラの最適化などで嬉しいケースも有る. 例えば, CPSにおいては, 関数呼び出しは必ず末尾呼び出し, つまりただのジャンプとして扱うことができる.

よくある例だが, 階乗を求める例をCPSで書いてみる. 通常のコードは以下のようになるだろう.

.. code-block:: hs

    fact :: Int -> Int
    fact 0 = 1
    fact n = n * fact (n - 1)

これをCPSにするには, 帰り値を受け取る関数を追加してやる.

.. code-block:: hs

    factCont :: Int -> (Int -> a) -> a
    factCont 0 k = k 1
    factCont n k = factCont (n - 1) $ \t -> k (n * t)

`factCont` を再帰呼び出しするときに, そこの継続に何を渡すかというのが少し考えるところではあるが, `\t -> k (n * t)` のように, 新たにクロージャを作って渡してやれば良い. `t` には `n-1` の階乗が計算されたものが入ってくるはずなので, それに `n` を掛けたものを自分の引数の継続 `k` に渡して完成.

毎回クロージャを作る時点で, このほうが効率悪いように思うかもしれないが, コンパイラが十分に賢ければ, このクロージャを作るのは回避されることが期待されていて, なおかつ再帰呼出しがジャンプに書き換えられ, このコードは理想的には極めて効率よく実行される「可能性がある」といえる. 「可能性がある」とは, Haskellのコンパイラはこれをたまにうまくやってくれないことがあり, でも気分が良ければ調子よくやってくれる. コンパイラの気持ちは複雑で, 人間は常にもてあそばれてしまう.

また, このCPSへの変換は通常, 機械的に行うことができる. 機械的にCPSに変換した後に, アグレッシブな最適化をかけることで, 正しい末尾再帰の実装を特別な枠組みなしに実現したり, 関数呼び出しの最適化がやりやすくなったりしてコンパイラ自体の単純化にも寄与する.

継続モナド
============

長くなったが, いよいよ継続モナドの出番である.

継続モナドというものが, mtlの `Control.Monad.Cont` に定義されている.

.. code-block:: hs

    class Monad m => MonadCont m where
      callCC :: ((a -> m b) -> m a) -> m a

`MonadCont` が継続モナドのクラスで, `callCC` というメソッドを一つだけ持つ. `callCC` が何かと言うと, 詳細は省く(`call-with-current-continuation`)が, 呼ばれた時点の継続を引数の関数に渡してくれるというもの.

`callCC` は, CPSではない, 普通のスタイルのコンテキストにおける継続を取りだして, プログラマに見せるという機能を提供する. 逆に, 継続モナドは内部的にはそのようなことが可能な実装になっていなければならないということである. 実装はイロイロ考えられるし, 普通のスタイルで実行をして, 命令ポインタとスタックフレームにコピーをして云々というものももちろんありえるだろう. しかし, 最も直接的な実装は, CPSの関数をモナドのインスタンスにしてしまうことである.

`MonadCont` のデフォルトの定義, `ContT` の定義を見てみる.

.. code-block:: hs

    newtype ContT r m a =
      ContT { runContT :: (a -> m r) -> m r }

モナド変換子版を提供するために, `ContT` はベースのモナドの型 `m` を引数にとる. 簡単のために, `m` に `Identity` を代入した非変換子版の `Cont` を考えてみる.

.. code-block:: hs

    newtype Cont r a =
      Cont { runCont :: (a -> r) -> r }

`Cont` は `(a -> r) -> r` のnewtypeである. これは直前の計算の結果 `a` を受け取って, 残りの計算を行った結果 `r` を返す関数を受け取る.
まさに, CPSスタイルの関数である.

同様に, `ContT` は, その結果をモナド `m r` に限定したものと考えることができる. 型に従って考えると, 単にそういうことになる.

ここで, 最初にでてきたファイルを開く関数を振り返り, `ContT` と並べている

.. code-block:: hs

    withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

    ContT :: { runContT ::  (a -> m r) -> m r }

型パラメータ `a` に `Handle` を, `m` に `IO` を代入して, ついでに列を揃えてみる.

.. code-block:: hs

    withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
    ContT { runContT ::               (Handle -> IO r) -> IO r }

このように, `withFile` の前二つの引数を覗いた型が見事一致した. `withFile` はモナド変換子版の継続モナドそのものだったということになる.

あとは簡単で, `ContT` を作るだけでモナドのインスタンスになる. 具体的に確認してみよう. 最初のファイルコピーのコードを例に取りあげる.

.. code-block:: hs

    copyFile :: FilePath -> FilePath -> IO ()
    copyFile from to = do
      withFile from ReadMode $ \h1 ->
        withFile to WriteMode $ \h2 ->
          content <- hGetContents h1
          hPutStrLn h2 content

これを継続モナドを使って書き直す.

.. code-block:: hs

    copyFile :: FilePath -> FilePath -> IO ()
    copyFile from to = (`runContT` return) $ do
      h1 <- ContT $ withFile from ReadMode
      h2 <- ContT $ withFile to WriteMode
      content <- liftIO $ hGetContents h1
      liftIO $ hPutStr h2 content

`withFile` に `ContT` をつけることによって, 継続渡しの関数があたかも `openFile` のような通常の値を返す関数であるかのように使えている. しかもエラー時を含めて, ブロックを抜けたときに, きちんとリソースの解放まで行われている.

`ContT` モナドのままだと実行できないため, これを実行できるように `ContT` を外してやる必要がある. これは newtype コンストラクタである, `ContT` を外して中の値を取りだしてやるだけである. `('runContT' return)` の部分がそれに相当する. `runContT` の型を書き出してみると, 次のようになっている

.. code-block:: hs

    runContT :: ContT r m a -> (a -> m r) -> m r

`runContT` はnewtypeのフィールド名だが, これを普通の関数と考えると, 継続モナドを受け取って, CPSの関数を返す関数と見ることができる. 要するに, `ContT` をつけるとCPSから普通のスタイルに変わって, `runContT` で普通のスタイルからCPSに戻るというイメージである. とはいえ, 理屈では分かっていても, 実際に起こっていることはかなり不思議である. ただ, 型があっているんだからしょうがない.

次は, `catFiles` 関数を継続モナドで書き直してみる. 複数のリソースの確保が悩ましい問題だった. 以下に再掲する.

.. code-block:: hs

    catFiles :: [FilePath] -> FilePath -> IO ()
    catFiles froms to =
      withFile froms ReadMode $ \fromhs ->
        withFile to WriteMode $ \toh ->
          forM_ fromhs $ \fromh -> do
            content <- hGetContents fromh
            hPutStr toh content

`ContT` によって通常のスタイルにしてしまえばなんのことはない.

.. code-block:: hs

    catFiles :: [FilePath] -> FilePath -> IO ()
    catFiles froms to = (`runContT` return) $ do
      fromhs <- forM froms $ from -> ContT $ withFile from ReadMode
      toh <- ContT $ withFile to WriteMode
      forM_ fromhs $ \fromh -> do
        content <- liftIO $ hGetContents fromh
        liftIO $ hPutStr toh content

`forM` で繰り返してやるだけである. 目的のために, 今や普通のモナドを扱う関数を利用することができる. リスト向けに `withMany` などというものを使ったり, あれこれ痒いところに手が届かない合成関数を揃えていく必要もない. なにせモナドだから, 何も考えずにモナドに身を委ねればよい.

速度が気になる方もいるかも知れないが, これもCPS変換での最適化と同様に(同じではないが), コンパイラが理想的な振る舞いをしてくれれば, 最適化され, 完全に元のコードと同じ性能が出ることが期待される. そして, GHCの新バージョンならおそらくそのような振る舞いをしてくれることだろう.

他のリソース管理に特化したモナド
==================================

継続モナドをリソース管理に使えるという話をしたが, Hackageにはいくつかリソース管理を目的としたモナドを実装しているパッケージがある.

Managed モナド
---------------

`managed` パッケージで定義されている, `Managed` モナドは, `ContT` と全く同じ型で, 振る舞いも同じだが, リソース管理という目的における, ユーザーフレンドリーな名前が付いているという点と, リソース管理に用いるために便利な関数やインスタンスが追加されているというのがモチベーションのようだ.

例えば, `Managed` 型に対して, `Monoid` のインスタンスが定義されていたり, `Num` を始めとする直接数値として扱うためのインスタンスが定義されている.

ResourceT モナド
------------------

当然ながら, リソース管理のための抽象化が一通りということはありえないし 一通りである必要もないため, 別のアプローチも考えられる.

`resourcet` パッケージで定義されている, `ResourceT` モナドはずいぶんと趣向が変わって, リソース解放のための関数を集めた配列を持ち回りながら, 最後にその配列の関数を実行するモナドである. リソース確保の際に, リリースするためのキーを返すような機能があり, 必要があれば計算の途中でもリソースを開放できるような仕組みになっている.

プログラマが望めば, 必要なくなったソケットをすぐさま解放するなどのような処理を簡単に記述できるが, もちろんそういう仕組を使うときはプログラマ自身が解放済みリソースに触れないように気をつけなければいけない.

また, そのようなリソースの解放を明示的に行える機能を提供する以上, 分離されたリソース確保関数と解放関数が必要になるため, with系の関数とは同時に使用することができない.

この辺りはアプリケーションによってトレードオフになるだろう. `ResourceT` モナドは `Yesod` で使うために作られた. `Yesod` はWebフレームワークなので, できるだけ早いタイミングでソケットなどを解放することが重要なようで, 逆にそういうことが必ずしも必要で無いなら, 安全性と利便性で継続モナドを使うことがまさるケースも考えられている.


