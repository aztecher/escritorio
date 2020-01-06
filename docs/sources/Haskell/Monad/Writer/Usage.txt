=====================
Writer Monad Usage
=====================

基本的な記述方法や例題を示す.

Example. 計算の結果を得る
============================

1から10まで足し算するコードを考えてみる.
関数型言語では高階関数を使ってすっきり表現できる.

.. code-block:: hs

    main = do
      print $ sum' [1..10]

    sum' :: Num a => [a] -> a
    sum' xs = foldl (+) 0 xs

手続き型言語(ex. Python)で書かれたコードならこうなるだろう

.. code-block:: python

    def sum (xs):
        summary = 0
        for n in xs:
            summary = summary + n
        return summary

ただし, この例での関数型で書かれたコードには欠点がある.
最後の結果を結果を求めるだけならこれでよいが, 足し算の経過を見たい時どうすればいいか分からない

手続き型言語の例でいれば, これを実現するには一行加えれば良い.

.. code-block:: python

    def sum (xs):
        summary = 0
        for n in xs:
            summary = summary + n
            print (summary)
        return summary

さて, これを解決する方法を探っていこう.

Method1. IOモナドを使う
-------------------------

`(+)` のIOモナド対応版(`addIO`)を作成し, `foldl` を `foldM` に変更することで, `addIO` 内で `putStrLn` が使えるようになる


.. code-block:: hs

    main = do
      print =<< sumIO [1..10]

    sumIO :: (Num a, Show a) => [a] -> IO a
    sumIO xs = foldM addIO 0 xs

    addIO :: (Num a, Show a) => a -> a -> IO a
    addIO p1 p2 = putStrLn ("r: " ++ show r) >> return r
      where
        r = p1 + p2

ただ, これだと純粋な関数ではなくなり, IOを引きずっている箇所でしか利用できない.

Method2. traceを使う
----------------------

Haskellには純粋な関数内で計算をデバッグ出力する関数が用意されている. `trace` は第一引数を標準出力に表示し, 第二引数と同じものに評価される関数である.

.. code-block:: hs

    import Debug.Trace

    main = do
      print $ sumT [1..10]

    sumT :: (Num a, Show a) => [a] -> a
    sumT xs = foldl addT 0 xs

    addT :: (Num a, Show a) => a -> a -> a
    addT p1 p2 = trace ("r: " ++ show r) r
      where
        r = p1 + p2

場合によってはこれで十分だろう. ただ, 経過の値を他の計算でも利用したいときにい, 標準出力に表示されてしまったものを利用することはできない.

Method3. Writerモナドを使う
-----------------------------

ここでWriterモナドの登場である. WriterモナドはIOモナドとは違い, 純粋関数内で実行でき, コードに下記のように手を加えることで, 結果にいたるまでの過程の値を最後にリストとして得ることができる.

.. code-block:: hs

    main = do
      print $ sumW [1..10]

    sumW :: (Num a) => [a] -> (a, [a])
    sumW xs = runWriter $ foldM addW 0 xs

    addW :: (Num a) => a -> a -> Writer [a] a
    addW p1 p2 = tell [r] >> return r
      where
        r = p1 + p2
