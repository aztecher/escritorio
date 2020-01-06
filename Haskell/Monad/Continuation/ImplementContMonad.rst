======================
Contモナドを実装する
======================

:doc:`./ContStyleWithoutMonad` の続き.

上の記事では, モナドの登場しない継続の説明を書いた.
今回はこの時使った継続を Contモナド というモナドに変換していく.

前回定義したaverage3CPSの定義を見直す.

.. code-block:: hs

    addCPS :: Num a => a -> a -> (a -> result) -> result
    addCPS x y cont = cont (x + y)

    divCPS :: Fractional a => a -> a -> (a -> result) -> result
    divCPS x y cont = cont (x / y)

    average3CPS :: Fractional a => a -> a -> a -> (a -> result) -> result
    average3CPS x y z cont =
      addCPS x y $ \xPlusY ->
      addCPS xPlusY z $ \xPlusYPlusZ ->
      divCPS xPlusYPlusZ 3 $ \average ->
      cont average

さて, average3CPSの定義を見てみると, `hoge a b c $ \z -> ...` という形のボイラープレートが繰り返されていることに気づく.
このような構文上のボイラープレートはモナド化のチャンスなので覚えておくといい.

average3CPSに現れるそれぞれの$を見てみると, その左側は以下のような型になっていることがわかる.

.. code-block:: hs

    ($)の左側 :: (a -> result) -> result

(* ここでのaは, average3CPSの型定義に出てくるものとは別物である)

このうち, resultの部分はプログラムの最終的な実行結果を表すために不変で, aの部分は例えば, `addCPS x y` や `divCPS xPlusYPlusZ 3` などの結果の型になる.

このことから察するに, Contモナドの構成要素は以下のような形になるはず.

.. code-block:: hs

    newtype Cont result a = Cont {
      runCont :: (a -> result) -> result
    }

事実として, これが Contモナド の具体的な型になる.

次に, returnの実装を考える.
returnは何もしない(>=>の単位元になる)ものなので, 今までの書き方で言えば以下のような関数にそうとうするものになるだろう.

.. code-block:: hs

    identityCPS :: a -> (a -> result) -> result
    identityCPS x cont = cont x

最後に, >>= の実装について考える. >>= は, 今までの書き方をすれば以下のような関数に相当する型を持つはず.

.. code-block:: hs

    bindCPS ::
      ((a -> result) -> result) ->
      (b -> ((c -> result) -> result)) ->
      ((c -> result) -> result)

一番最後の括弧は外しても変わらないので, 外して考える.

.. code-block:: hs

    bindCPS ::
      ((a -> result) -> result) ->
      (a -> ((b -> result) -> result)) ->
      (b -> result) ->
      result

このことから, このbindCPSは

1. a型の結果を継続に渡す関数xCPSを受け取り,
2. aを引数としてとり, 結果の型bの値を継続に渡す関数fCPSを受け取り,
3. b型の値を受け取って最終的な計算結果を返す継続を受け取り,
4. 最終的な計算結果を返す

関数であることがわかる.
さて, ではコード化してみる.

.. code-block:: hs

    bindCPS ::
      ((a -> result) -> result) ->
      (a -> ((b -> result) -> result)) ->
      (b -> result) ->
      result
    bindCPS xCPS fCPS cont =
      xCPS $ \xVal ->
      fCPS xVal $ \yVal ->
      cont yVal

最後に, これらを新しい型Contに合わせて書き換え, モナドのインスタンスにしてしまう.

.. code-block:: hs

    import Control.Monad (liftM, ap)
    
    newtype Cont result a = Cont {
      runCont :: (a -> result) -> result
    }
    
    returnCont :: a -> Cont result a
    returnCont a = Cont $ \cont -> cont a
    
    bindCont :: Cont result a -> (a -> Cont result b) -> Cont result b
    bindCont (Cont xCPS) f = Cont $ \cont ->
      xCPS $ \xVal ->
      let Cont yCPS = f xVal
      in yCPS $ \yVal ->
        cont yVal
    
    
    instance Monad (Cont result) where
      return = returnCont
      (>>=) = bindCont
    
    instance Applicative (Cont result) where
      pure = return
      (<*>) = ap
    
    instance Functor (Cont result) where
      fmap = liftM
    
    -- define average3CPS by using Cont Monad
    
    addCont :: Num a => a -> a -> Cont result a
    addCont x y = return $ x + y
    
    divCont :: Fractional a => a -> a -> Cont result a
    divCont x y = return $ x / y
    
    average3Cont :: Fractional a => a -> a -> a -> Cont result a
    average3Cont x y z = do
      xPlusY <- addCont x y
      xPlusYPlusZ <- addCont xPlusY z
      let average = xPlusYPlusZ / 3
      return average

実行は以下のように行う.

.. code-block:: hs

    ghci> runCont (average3Cont 3 4 5) $ \x -> x
    4.0
    ghci> runCont (average3Cont 3 4 5) $ \x -> show x
    "4.0"

継続をモナドにしたメリットは, これまで明示的にcontを受け渡していたのをうまく隠蔽することができるところにある. (某氏の言っていた, 「モナドはデザインパターンですよ」という言葉の意味がよく分かる)

さて, 新旧の書き方がどう変わっているのか再確認してみる

.. code-block:: hs

    -- 旧 (モナドでない継続)
    average3CPS :: Fractional a => a -> a -> a -> (a -> result) -> result
    agerage3CPS x y z cont =
      addCPS x y $ \xPlusyhY ->
      addCPS xPlusY z $ \xPlusYPlusz ->
      divCPS xPlusYPlusZ 3 $ \average ->
      cont average

    -- 新 (モナドによる継続)
    average3Cont :: Fractional a => a -> a -> a -> Cont result a
    average3Cont x y z = do
      xPlusY <- addCont x y
      xPlusYPlusZ <- addCont xPlusY z
      let average = xPlusYPlusZ / 3
      return average

いままでの書き方では, 

.. code-block:: hs

    hogeCPS $ \x -> ...

の...の部分が, 最終的な計算結果を得るまでの残りの処理, すなわち継続を表していたが, 新しい書き方では

.. code-block:: hs

    x <- hogeCont
    ...

の...の部分, すなわち, アクションの実行以降の行が暗黙的に継続として扱われる.
これによって, 現在の継続をより簡単に扱うことができるようになった.
