===========
存在量化
===========

このページでは, `Existential Type` ないし `Existentials` と呼ばれる存在型に関して記述していく.
これは, 多相型などとも密接に関わってくる.

全称量化
=========

全称量化は, Haskellで多相性を実現する根幹となる仕組みである.
全称量化の本質は,
「型の集合に対して同じ方法で操作し, 振る舞いが扱っている範囲のすべての型の振る舞い `のみ` により定められるような関数を表現できること」
である

.. code-block:: hs

    {-# LANGUAGE ExplicitForAll #-}

    example1 :: forall a. [a]
    example1 = []

    example2 :: forall a. [a]
    example2 = [undefined]

    map' :: forall a. forall b. (a -> b) -> [a] -> [b]
    map' f = foldr ((:) . f) []

    reverse' :: forall a. [a] -> [a]
    reverse' = foldl (flip (:)) []

通常, 量子化は型シグネチャでは省略される.
なぜなら, Haskellのありきたりな表層言語では, 自由な `束縛されていない` 型変数は全称量化すると仮定しても曖昧さが生じないからである.
つまり, 上記のコードは通常このようになる.

.. code-block:: hs

    example1 :: [a]
    example1 = []

    example2 :: [a]
    example2 = [undefined]

    map' :: (a -> b) -> [a] -> [b]
    map' f = foldr ((:) . f) []

    reverse' :: [a] -> [a]
    reverse' = foldl (flip (:)) []

これを見ると, いつもどおりである. ただその裏に `forall` などが隠れているということは知っておく.

===============
自由定理
===============

全称量化された型変数は実は, 関数の実装についてかなり多くの深淵な性質を示唆している.
この性質は関数の型シグネチャから導ける.
例えば, Haskellの恒等関数は, 型シグネチャに対する実装を一つしか持たないことが保証されている.

.. code-block:: hs

    id :: forall a. a -> a
    id x -> x

.. code-block:: hs

    fmap :: Functor f => (a -> b) -> f a -> f b

fmapに対する自由定理が以下

.. code-block:: hs

    forall f g. fmap f . fmap g = fmap (f . g)

===============
型システム
===============

今は省略. 必要ならお勉強.

=============
存在量化
=============

全称量化の本質は, 「いかなる型に対しても同じ方法で操作する関数を表現すること」である.
一方, 存在量化は, 「ある未知の型に対して走査する関数を表現」できる.
存在量化を使えば, 存在量化の下にある, 「データ型を操作するが型シグネチャがその情報を隠している関数」を使って,
異種の複数の値をまとめて扱うことができる.

.. code-block:: hs

    {-# LANGUAGE ExistentialQuantification #-}
    {-# LANGUAGE RankNTypes #-}

    -- exist t. (t, t -> t, t -> String)
    data Box = froall a. Box a (a -> a) (a -> String)

    -- Box data : t = Int
    boxa :: Box
    boxa = Box 1 negate show

    -- Box data : t = String
    boxb :: Box
    boxb = Box "foo" reverse show

    apply :: Box -> String
    apply (Box x f p) = p (f x)


    -- exist t. Show t => t
    data SBox = forall a. Show a => SBox a

    boxes :: [SBox]
    boxes = [SBox (), SBox 2, SBox "foo"]

    showBox :: SBox -> String
    showBox (SBox a) = show a

    main :: IO ()
    main = mapM_ (putStrLn . showBox) boxes

`SBox` による存在量化により, Showのインターフェースにより純粋に定義されたいくつかの値を集められるが, 値についての他の情報は手に入らず, 他のいかなる方法でもアクセスしたりアンパックすることはできない.

.. code-block:: hs

    {-# LANGUAGE RankNTypes #-}

    -- この関手はライブラリの内部実装を修正したものである.
    type Exists a b = forall f. Functor f => (b -> f b) -> (a -> f a)

    type Get a b = a -> b
    type Set a b = a -> b -> a

    example :: Get a b -> Set a b -> Exists a b
    example f g l a = fmap (g a) (l (f a))

全称量化を使うと, いわゆる「オブジェクト指向パラダイム」と言われるある種の概念を再現できる.
これは80年代後半に人気を博した学派で, 現代的な等式で再現する方法を使わず,
プログラミングの理論を人間らしい実体と動作へと分離することを試みたものである.
このモデルを Haskell で再現することは, 広くアンチパターンであると考えられている.

==========
不可述型
==========

興味があれば書く.
GHCではまだ恐ろしく不安定らしいが, 型変数を多相型で実体化できるというよくわからないことができる.

======================
スコープのある型変数
======================

通常, 関数のトップレベルのシグネチャの内部で使われている型変数は, 型シグネチャ内部でのみスコープを持ち, 関数本体ではスコープを持たず, 項や let/where 節に対しては固定のシグネチャである.
`-XScopedTypeVariables` を有効にすると, この制限が弱まり, トップレベルで言及された型変数が, 値のレベルの関数本体とそこに含まれるすべてのシグネチャの内部にスコープを持つようになる.

.. code-block:: hs

    {-# LANGUAGE ExplicitForAll #-}
    {-# LANGUAGE ScopedTypeVariables #-}

    poly :: forall a b c. a -> b -> c -> (a, a)
    poly x y z = (f x y, f x z)
      where
        -- 2番目の引数は型推論により全称量化されている
        -- f :: forall t0 t1. t0 -> t1 -> t0
        f x' _ = x'

    mono :: forall a b c. a -> b -> c -> (a, a)
    mono x y z = (f x y, f x z)
      where
        -- bは ---ここでミスってweb更新

