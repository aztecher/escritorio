=================================================
Hyper-Order Polymorphism
=================================================

高階多層型に関して例を交えながら説明してみる.

なんでも入るリスト
====================

リストの要素がなんでもいいものを考えてみる. これはJavaScriptで実装することができる.

.. code-block:: js

    var array = [1, 2, "three", false, {"five": 6}];

さて, これを Haskell で同様に表現しようとすると怒られてしまう.

.. code-block:: hs

    list = [1, 2, "three", False, ("five", 6)]
    >> (Error Message...)

さて, これどうにかして型付けしたいという欲求が出てくるわけである.

ここで登場するのが, **高階多層型** である.

.. code-block:: hs

    {-# LANGUAGE RankNTypes #-}
    {-# LANGUAGE ExistentialQuantification #-}

    data Any = forall a. Any a

    list :: [Any]
    list = [Any 1, Any 2, Any "three", Any False, Any ("five", 6)]

これらは表示はできないがきちんとロードができる.

さて, ここで「騙されんぞ! ただデータ構築子で包んで隠蔽しただけじゃねーか」と考えれたらセンスがいい. 似たようなデータ構築師として, `Maybe` を試してみよう. すなわち以下が実行できればこれは別段新しくもなんともないわけである.

.. code-block:: hs

    list = [Just 1, Just 2, Just "three", Just False, Just ("five", 6)]

しかしこれは先程と同じエラーがでる. 型を考えてみると当然だが, これは `Just 1` の場合は, ``Num a => Just a`` であり, `Just "three"` は, ``Just [Char]`` となるのがわかるだろう. すなわち, データ構築子で包んだとしても型が違うわけだ.

さて, では改めて, `Any` で成功した理由を考えてみる. ピンときている人は「隠蔽」が全てだと分かっているだろうが, 以降もう少し正しい言葉遣いで説明してみることにする

それぞれの型は次のようであった

.. code-block:: hs

    data Any = forall a. Any a
    data Maybe a = Nothing | Just a

さて, このデータ型に関して **カインド** を考えるとはっきりする.

.. code-block:: hs

    ghci> :k Any
    Any :: *
    ghci> :k Maybe
    Maybe :: * -> *

`Maybe` の場合は, `Nothing` ないしは `Just a` の値を取るが, このときの `a` の型は `Maybe a` の型として現れる. それに対し, `Any` は `forall a. Any a` を値として取る. 「任意の型aに対して, `Any a` は Any型の値になる」のである. つまりこのような場合は問題となる型の違いが裏に隠蔽され, コンパイラに怒られないということである.


とまぁ, ここまではいいが, 実際に使うときは少し困ったことに成る. すなわち, 値が何かよくわからないということである. Anyがくるんだ値はパターンマッチで取り出すとして, その取り出し後の値が一体何なのか分からないから使いようがないという問題が出てくる. 数値として扱えるのか? 表示できるのか? `==` で評価できるか? とかが分からないわけだ.

そのため, 使えるようにするためにAny型に制約を付けるわけである.
すなわち, 「任意の型aに対して, `Any a` はAny型の値なんだが, aは常に〜を満たす」という感じである. この制約を与えるのは, forall である.

.. code-block:: hs

    {-# LANGUAGE RankNTypes #-}
    {-# LANGUAGE ExistentialQuantification #-}

    data Printable = forall a. Show a => Printable a

    instance Show Printable where
        show (Printable a) = show a

    list :: [Printable]
    list = [Printable 1, Printable 2, Printable "three", Printable False, Printable  ("five", 6)]

    main = print list

上記の例は制約として「この型は常に表示できる」という制約が付いている.
これでごちゃまぜのリストを作成した上に表示までできるようになった.
