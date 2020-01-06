==================================
モナドから初めない継続入門
==================================

継続モナド (Continuation Monad) の説明に取り掛かる前に, まずその前段として「継続」の話をする.

まぁ, この「継続」というのが「モナド」というのが「継続モナド」というのは言葉ずらから想像できるので, 根本的に「継続」を理解していないといかんでしょということだ.

継続
=====

Haskell(に限らずほとんどの言語)では, 以下のようなシステムでプログラムを実行している.

1. 何かしらの関数に引数を与えて, その計算結果を受け取る.
2. 受け取った計算結果を別な関数の引数に与える
3. 以下, 繰り返し.

例えばHaskellで, 与えられた3つの数の平均をとる関数を(非常に冗長に)書いてみると, 以下のようになるだろう

.. code-block:: hs

    import Prelude hiding (div)

    add :: Num a => a -> a -> a
    add x y = x + y

    div :: Fractional a => a -> a -> a
    div x y = x / y

    average3 :: Fractional a => a -> a -> a -> a
    average3 x y z =
      let xPlusY = add x y
          xPlusYPlusZ = add xPlusY z
          average = div xPlusYPlusZ 3
      in average

このような通常の書き方を, 「直接スタイル」といいます.
これは「何らかの関数に引数を与えて, その計算結果を受け取る」というのが基本の操作になる.

これに対して, 関数が「その関数自体の実行が終わった後の残りの処理」を引数として受け取り(つまり, そのような残りの処理を行う関数をも受け取り), それに明示的に計算結果を渡していくような書き方を「継続渡しスタイル」といい, 「関数の実行が終わった後の残りの処理」のことを継続という.

継続渡しスタイルの書き方
==========================

それでは, 先程作ったaverage3を, 継続渡しに書き換えてみよう.
以降では, 関数hogeの継続渡しバージョンをhogeCPS(Continuation-Passing Style)という名前で定義する.

Haskellにおいて, 「関数の実行が終わった後の残りの処理」は, それ自体一つの関数で表すことができる.

まずは例として, addを継続渡しスタイルで書いてみる.

.. code-block:: hs

    addCPS :: Num a => a -> a -> (a -> result) -> result
    addCPS x y cont = cont (x + y)

さて, 雰囲気は分かったと思うので, 同様にdivも継続渡しスタイルに変換していく.

.. code-block:: hs

    divCPS :: Fractional a => a -> a -> (a -> result) -> result
    divCPS x y cont = cont (x / y)

最後に, average3CPSをaddCPSとdivCPSを使って書いていくが, その前にaverage3の定義を見直すと, 以下のような処理を行っている.

1. add x y の結果を xPlusY に束縛
2. add xPlusY z の結果を xPlusYPlusz に束縛
3. div xPlusYPlusZ 3 の結果を average に束縛
4. average を返し
5. average3 が返した値を使って, 呼び出し側がなにかする.

順に追って見ていく.
1の実行時点での継続は2, 3, 4, 5となり,
2の実行時点での継続は3, 4, 5となり,
3の実行時点での継続は4, 5となり,
4の実行時点での継続は5となる.

したがって, 1の実行時点の継続を考えると,
average3CPSは以下のような形をしているはずである.

.. code-block:: hs

    average3CPS x y z cont = 
        addCPS x y $ \xPlusY ->
        ...

この「...」の部分には, 2, 3, 4, 5の処理が入るはずである.
順を追って見ていく.

.. code-block:: hs

    aberage3CPS x y z cont =
        addCPS x y $ \xPlusY ->
        addCPS xPlusY z $ \xPlusYPlusZ ->
        divCPS xPlusYPlusZ 3 $ \average ->
        ...

これでx, y, zの平均を受け取るところまでは実装した.
最後にやらなければいけないことは, もちろん残りの継続contに計算結果を渡すことである.

というわけで, 以下がaverage3CPSの最終的な実装となる.

.. code-block:: hs

    average3CPS :: Fractional a => a -> a -> a -> (a -> result) -> result
    average3CPS x y z cont =
        addCPS x y $ \xPlusY ->
        addCPS xPlusY z $ \xPlusYPlusZ ->
        divCPS xPlusYPlusZ 3 $ \average ->
        cont average

さて, 動作確認をする.

.. code-block:: hs

    ghci> average3CPS 3 5 7 $ \avg -> show avg
    "5.0"
    ghci> average3CPS 3 5 7 $ \avg -> avg
    5.0

継続のメリット
===============

継続渡しスタイルのメリットはなんだろうか.
言ってしまえば, 意図して順番に関数に食わせれば, 直接スタイルだろうが継続渡しスタイルだろうが結果は変わらないし実行もできる.

継続渡しスタイルのメリットは, 「残りの処理の実行を関数側が制御できる」ことがあげられるだろう. 関数から帰った後の「残りの処理」はあくまでもその関数自体の引数なので, 呼ぶも呼ばないも自由だし, 呼んだ後に適当に結果に細工することだってできてしまう.

例えば, ゼロ除算の場合には残りの処理を実行せずに, エラーメッセージを返すようなsafeDivCPSを以下のように定義することができる.

.. code-block:: hs

    type ErrMsg = String
    safeDivCPS :: (Eq a, Fractional a) =>
      a -> a -> (a -> Either ErrMsg result) -> Either ErrMsg result
    safeDivCPS _ 0 _ = Left "division by zero"
    safeDivCPS x y cont = cont (x / y)

これに適当な継続を与えて実行してみよう.
継続がEither ErrorMsg resultの型を返さなければならないのに注意.

.. code-block:: hs

    ghci> safeDivCPS 3 2 $ \d -> Right ("the answer is " ++ show d)
    Right "the answer is 1.5"
    ghci> safeDivCPS 3 0 $ \d -> Right ("the answer is " ++ show d)
    Left "division by zero"

ゼロ除算の場合は残りの処理を実行せず, 直ちにLeft "division by zero"を返すようになります.

このように, 関数実行後の振る舞いも含めて制御できてしまうのが, 継続のメリットになる.
これを利用することで, 例えば以下の記事のように, Haskellでループからbreakするといったこともできる.
