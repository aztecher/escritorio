================================
Wizardモノイドとその仕組み
================================

GHC8.0からIO型がMonoidのインスタンスになった. これを利用した便利なテクニックを紹介する.

Wizardは「魔法使い」の意味だが, ソフトウェアをインストールするときとかに出てくる, インストール「ウィザード」のような対話形式での作業を誘導してくれるソフトウェアのことでもある. WizardモノイドのWizardは後者の意味で利用されている.

Wizardモノイド
==================

まずWizardモノイドを紹介をする. 以下のような例を考えてみる.

.. code-block:: hs

    main :: IO ()
    main = do
      -- 最初に全ての情報を入力してもらう.
      putStrLn "名前は?"
      name <- getLine

      putStrLn "年齢は?"
      age <- getLine

      -- 最後に全てのアクションを実行する.
      putStrLn ("名前: " ++ name)
      putStrLn ("年齢: " ++ age)

この短いプログラムには,

* 名前を聞いて最後に名前を表示する.
* 年齢を聞いて最後に年齢を表示する.

という明らかなパターンがあるが, それぞれの「聞く」と「表示する」が入れ子になっているせいでうまく分割ができない. そこで登場するのが, Wizardモノイドｍである. Wizardモノイドを使うと以下のように書ける.

.. code-block:: hs

    import Data.Monoid ((<>))

    name :: IO (IO ())
    name = do
      putStrLn "名前は?"
      x <- getLine
      return (putStrLn ("名前: " ++ x))

    age :: IO (IO ())
    age = do
      putStrLn "年齢は?"
      x <- getLine
      return (putStrLn ("年齢 : " ++ x))

    runWizard :: IO (IO a) -> IO a
    runWizard request = do
      respond <- request
      respond

    main :: IO ()
    main = runWizard (name <> age)

名前と年齢それぞれの処理をうまく分離することが出来ている. どの部分がWizardモノイドになっているのか敢えて型で定義すると, 以下のように成るだろう.

.. code-block:: hs

    type Wizard a = IO (IO a)

Wizardモノイドによってコードを分割したことで処理をまとめることができるようになった.

.. code-block:: hs

    import Data.Monoid ((<>))

    prompt :: String -> IO (IO ())
    prompt attribute = do
      putStrLn (attribute ++ "は?")
      x <- getLine
      return (putStrLn (attribute ++ ": " ++ x))

    runWizard :: IO (IO a) -> IO a
    runWizard request = do
      respond <- request
      respond

    main :: IO ()
    main = runWizard (prompt "名前" <> prompt "年齢")

もし追加で好きな色を訪ねたくなったら, 以下のように修正すればよろしい

.. code-block:: hs

    main :: IO ()
    main = runWizard (prompt "名前" <> prompt "年齢" <> prompt "好きな色")

入力 -> 出力のパターンだけでなく, アクション -> アクションのパターンなら何でも入れ子に合成できるため, Wizardモノイドは意外と使いみちが広く重宝しそう.

Wizardモノイドの仕組み
========================

なぜ， WizardモノイドがIOの入れ子の合成を実現できているかを見ていく. 秘密はIOのMonoidのインスタンスにある. IOのMonoidのインスタンスは以下のように定義されている.

.. code-block:: hs

    instance Monoid a => Monoid (IO a) where
      mempty = pure
      mappend = liftA2 mappend

`liftA2` は

.. code-block:: hs

    liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

と定義されている. `mappend` の定義を分かりやすく書き下すと以下のようになる

.. code-block:: hs

    mappend action1 action2 = do
      result1 <- action1
      result2 <- action2
      pure (result1 <> result 2)

要するにアクションを一つづつ実行して得られた結果の型のMonoidインスタンスの `mappend` で合成する. これを踏まえると, 先の例の `name <> age` は以下のような処理になっている.

.. code-block:: hs

    do
      response1 <- name
      response2 <- age
      pure (response1 <> response2)

`name` と `age` の実装を思い出すと, `response1` と `response2` はそれぞれ名前と年齢を表示する処理になっているはずである.

`runWizard` は `name <> age` を実行した後に更に最後の `response1 <> response2` を取りだして実行するという処理を行っている. これで, 「聞く」と「表示する」の処理を入れ子にすることができる.


