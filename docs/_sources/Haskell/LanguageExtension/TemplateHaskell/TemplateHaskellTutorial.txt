===========================
Template Haskell Tutorial
===========================

Motivation
===========

THにおける難しさの一つは, それが手元の問題に対する最善の解決策であるかどうかを判断すること, であると思う. コードを生成するコードは一般に特定の問題に対応できず, メタプログラミングが最後の手段として使用されるようになる. 本当かどうか分からないが, THはかなり人気があり, 他では達成できない問題を解決する貴重なスキルであるといえるだろう.

THの用途をいくつか挙げてみる.

* 型クラスインスタンスの自動導出は最も一般的なTHの使用用途である. 'generics'によって同じ問題が解決できるとしても, THベースの解決法と比べてコンパイル時間が長くなることが知られているため, aeson や lens のようなライブラリでのインスタンスの自動導出には望ましいメソッドである.

* Haskellで構築されたシステムに統合されたTH製DSLの作成に使える. yesodなどで利用されている.

* 無効な入力をコンパイル障害に変える型のコンパイル時構築

* 外部ファイルからのデータのコンパイル時読み込み/処理. コンパイル時にIOの実行を伴うが, 危険な機能を比較的気楽に使えるユースケースである.

THの注意点をいかに挙げておく.

* THのヘルパーはしばしば「魔法」を行うブラックボックスとみなされる. Q[Deg]型が何をしているかわからないだろうし, 何かをしているのかもしれない. THコードの意味の情報は主要なソースになるだろうからドキュメントを残すこと.

* THは, TH関数自体を定義する場所と, TH関数が使用されているファイルについての制限を課す.

The Q Monad
============

コードの生成はいくつかの機能(関数)を要求する.

* 補足されない新たなユニークネームを生成する能力

* 名前に関して検索する能力. 通常, 関数と型について知りたいのだが, モジュールについて学習し, 特定の型クラスｍのインスタンスのコレクションを取得する方法もある.

* 同一モジュール内のTHコードで共有されるカスタムステートを取得/作成する能力.

* コンパイル時にIOを実行する能力.

これらの機能はHaskellに置いてはモナドを利用することで達成可能である. したがって, THによって提供される全ての機能をホストする特別なモナドであるQ(quotationの略)があることはさほど驚くべきことではないだろう.


Splicing
=========

`Q a` 型をもつ唯一の目的は, aをHaskellプログラムで使うことである. aは中間のモナド式では何でも構わないが, 生成されたコードをHaskellソースファイルに挿入しようとしているときには, 以下の4つのオプションしかない.

* Declaration_ (宣言) 型の `Dec` は関数やデータ型のようなトップレベルのものを含む. 実際, いくつかの宣言を一度に生成したいことがある. この時, [Dec]型を利用する.

* Expression_ (表現) 型の `Exp` は, x + 1 や \x -> x + 1 などである. 恐らく最も一般的なものであろう.

* Type_ (型) 型の `Type` は, `Int`, `Maybe Int`, `Maybe` などである. この型は飽和(?)する必要はない. (任意の種類がある可能性がある).

* Pattern_ (パターン) 型の `Pat` は, パターンマッチングに利用する.

上記のリンクで `Dec`, `Exp`, `Type`, `Pat` についての定義を確認しておくといい. コンストラクタのサフィックス(接尾語)にはそれぞれの型のイニシャルがついている.

データ型を利用することで, **ゆっくりと**, **痛みと苦痛を伴いながら**, 表現を構築することができるようになる. (恐らくこのような書き方はしないよてきなことだろう)

.. code-block:: hs

    myFunc :: Q Exp
    myFunc = do
      x <- newName "x" -- generate a unique variable name, well
      return $ LamE -- lambda expression
        [VarP x]
        (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))
        -- here we have an infix expression:
        -- we apply (+) to 'x' and integer literal 1

THによって特別なシンタックス `$(exp)` が利用できる. ここで `exp` は `Q[Dec]`, `Q Exp`, `Q Type`, `Q Pat` で生成される任意の表現を示している. これは通常のHaskellコードの中に生成されたコードを書き加える.

例として先程書いた, `myFunc` を実行してみる.

.. code-block:: hs

    > :set -XTemplateHaskell
    > $(myFunc) 3
    4

    -- myFuncが引数を取らない場合は括弧は必要ない.
    -- もし引数を取る場合は, $(myFunc arg) 3 のようなものになる.
    -- 言い換えるならば, 括弧は式の周りにのみ必要である
    > $myFunc 3
    4
    > let f = (* 2) . $myFunc
    > f 10
    22

これは `splicing` と呼ばれる. ドルマーク以降の表現を `splice(より継ぎする/継ぎ合わせる)` という. spliceは式, パターン, 型の代わりにトップレベルの宣言として行うことができる.
(これ以降ドルマークとspliceに関しての言及があるがよく分からなかった)

Limitations of TH
==================

現在, THを使用する場合以下の制約がある.

* **ステージング制限** とは, splice内で既にコンパイルされた, つまり他のモジュールで定義された(spliceを含む同じモジュール下でない)関数が使用できることである. これは, 開発者にTHと呼ばれるTHコードをモジュールから切り離すようにさせる厄介な制限である. (つまるところ, THコード内で利用する関数は別モジュールとして切り出さないといけないという制限. 少し手間)

* THではしばしば, 定義の順序付けを強制する事があるらしい. (詳細は記述しないが, つまるところコードを書く位置を強制されることがあるらしい)

`lens` ライブラリを使用した例を以下に示す.

.. code-block:: hs

    data MyRecord = MyRecord -- <<< first declaration group
      { _myRecordFoo :: Foo
      , _myRecordBar :: Bar
      , _myRecordBaz :: Baz
      }

    getRecordFoo :: MyRecord -> Foo
    gerRecordFoo = view myRecordFoo

    makeLenses ''MyRecord -- << second declaration group
    -- ^ Generates lenses: 'myRecordFoo', 'myRecordBar', and 'myRecordBaz'

悲しいことにこのコードはコンパイルできない. 第一宣言グループは `MyRecord` と `getRecordFoo` の定義を含んでいるが, `myRecordFoo` は `getRecordFoo` のスコープ外であるがゆえにlensesをジェネレートできない. これを解消するには, `getRecordFoo` を `makeLenses ''MyRecord` spliceの後に記述する.

.. code-block:: hs

    data MyRecord = MyRecord -- <<< first declaration group
      { _myRecordFoo :: Foo
      , _myRecordBar :: Bar
      , _myRecordBaz :: Baz
      }

    makeLenses ''MyRecord -- << second declaration group

    getRecordFoo :: MyRecord -> Foo -- can see 'MyRecord' from the
    getRecordFoo = view MyRecordFoo -- previous group

この様に第一宣言グループにデータ型, その後に `makeLenses` をしてから `getRecordFoo` を記述という流れになるらしい.

Quotation(引用)
==================

THがビルドし操作できるHaskellのASTは, コンパクトでなく, また使うのも簡単ではない. 残念なことに, コンパイルするHaskellプログラムが表さない正しい形のASTを作成することも可能である. 言い換えるなら, ASTの手動構築は退屈でエラーを起こしやすい.

幸運なことに, `quotation` を使って任意HaskellコードをASTで得る方法がある. TemplateHaskell言語拡張で以下の4つのquotationの方が利用できる

+----------------+------------------+---------+
| Thing produced | Quotation syntax | Type    |
+================+==================+=========+
| Declaration    | [d | ... | ]     | Q [Dec] |
+----------------+------------------+---------+
| Expression     | [e | ... | ]     | Q Exp   |
+----------------+------------------+---------+
| Type           | [t | ... | ]     | Q Type  |
+----------------+------------------+---------+
| Pattern        | [p | ... | ]     | Q Pat   |
+----------------+------------------+---------+

実際, 同じコードでは異なるコンテキストが異なることを意味する可能性があるため, いくつかことなるクォータが必要. 例えば.

.. code-block:: hs

    > runQ [e| Just x |] -- an expression
    AppE (ConE GHC.Base.Just) (UnboundVarE x)
    > runQ [p| Just x |] -- a pattern
    ConP GHC.Base.Just [VarP x_0]

多くの場合, `expression` を一番利用するため, 軽量クォートシンタックス `[|...|]` が定義されている(これは, `[e| ... |]` と同じ)

.. code-block:: hs

    > runQ [| Just x |] -- an expression
    AppE (ConE GHC.Base.Just) (UnboxedVarE x)

quotationができることは, Haskellコードをすばやく表現することだけではなく, 手動でASTを構築することもできる.

.. code-block:: hs

    myFunc :: Q Exp
    myFunc = [| \x -> x + 1 |]

この `myFunc` は理解しやすいだろう. クォータの最も素晴らしいことは, それらの中でspliceが利用できることである.

.. code-block:: hs

    add2 :: Q Exp
    add2 = [| $myFunc . $myFunc |]

このようにして, アルゴリズム的に変更する必要があるコードを変更するためにスプライシングを利用して, ほぼいつもどおりに生成したいコードを書くことができる. ただし, GHC 8.2.2以降, 宣言クォート内の宣言のspliceはまだ機能しない

.. code-block:: hs

    > $add2 10
    InfixE
      (Just (LamE [VarP x_2] -- lambda
        (InfixE (Just (VarE x_2))
          (VarE GHC.Num.+)
          (Just (LitE (IntegerL 1))))))
      (VarE GHC.Base..) -- functional composition
      (Just (LamE [VarP x_3] --lambda
        (InfixE (Just (VarE x_3))
          (VarE GHC.Num.+)
          (Just (LitE (IntegerL 1))))))

これは完全に意図したとおりに動作する

`runQ` とは何をしているのだろう? GHCiではIOモナドで動作するため, 上記の例では方が必要であると仮定するのは自然である

.. code-block:: hs

    runQ :: Q a -> IO a
            ^      ^
    --      |      | 
    --  we have  but we want
    --   this      this

`runQ` は通常, GHCiでTHを再生するために使用される. (これについては後ほど説明する).

さて, 少し掘り下げてみるともうすこしこれは複雑になっている.

.. code-block:: hs

    runQ :: Quasi m => Q a -> m a

`Quasi` はモナドの型クラスであり, 初めに `Q` について言及したメタプログラミングの能力を提供する. 再度確認してるといい. 実際, `Q a` は `Quasi m => m a` のラッパーである.

.. code-block:: hs

    newtype Q a = Q { unQ :: forall m. Quasi m => m a}

    runQ :: Quasi m => Q a -> m a
    runQ (Q m) = m

`Q` , `IO` というユーザーに見えるようにするための２つの `Quasi` インスタンスがある. `Q` のためのインスタンスは些末なものであり, `IO` のためのインスタンスは機能が非常に限られている. Quasiには多数のメソッドがあるが, `newName`, `runIO`, `reportError`, `reportWarning` の4つのみがサポートされている. そのため, 我々はちょうど見て確かめるだけのデバッグ目的にしか, IOを利用することができない.

このような `Q` の定義は, THの作者が具体的なモナドの中で働くことを望んでいたと同時に, すべての作業を行う `Quasi` インスタンスを定義することを望んでいたことを示唆している.

Names
======

我々は, 同一の名前が使用されるコンテキストに応じて異なるものを参照することができるということをよく知っている. コードの生成や操作を行う時, 我々は次に2つのタイプの名前を使用する.

* 現在のコンテキスト内の何かしらを意味する名前. 「現在のコンテキスト」とは我々が作成しようとしているメタプログラムのコンテキストであるかもしれないし, そのspliceの中かもしれない. どちらにせよ, 現在のスコープで何かしら動作するものとしての名前がある.

* 現在のコンテキスト内では一致しない名前. 例えば, lambda式を作成するとき, その引数をバインドすることができる. そのためには「名前」が必要である. このタイプの名前は, 更に２つのサブグループに分割できる.

    * 捕捉される名前. すなわちコードの生成後に細く可能な名前が含まれているかどうか.
    * 捕捉されない名前.


まずはじめに, 関数と型の名前を引用するための構文がある
(TemplateHaskellで有効)

* 関数名を引用する場合, シングルクオート先頭につける.
    `id` -> `'id`

* 型を引用する場合, ２つのシングルクオートを先頭につける.
    `MyRecord` -> `''MyRecord`

これらはHaskellが値と型で別のネームスペース持っていることに影響している.
また, データコンストラクタは `'MyRecord` 型コンストラクタは `''MyRecord` で引用する.

この手法は常に現在のスコープから参照できる名前を生成してくれる.
`makeLenses :: Name -> Q [Dec]` がよい例である.
これでは, レコード名である `''MyRecord` を渡している.
また, `myFunc` の最初の定義でも見たように, `(+)` 関数も引用されている.

.. _code-block: hs
    InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1)))
                                 ~~~~

この関数を定義した時, `(+)` 関数は `Prelude` から来ており, それはこのスコープ上である. そのため, `'(+)` で引用する.

引用を利用する場合, 完全に同様の動作をする必要がある.
THのクオートの全ての名前はカレントスコープにある必要がある.
言い換えるなら, 引用を1つ使用するときに使用しているスコープが, 結果としてASTで得られるものを決定している.(そのスコープに縛られる)

.. _code-block: hs

    > :set -XTemplateHaskell
    > :import Language.Haskell.TH
    > runQ [| x |]
    UnboundVarE

`x` がGHCiセッションで定義されていないと, `UnboundVarE x` を得る
しかし, 初めに `x` を定義し, 同様のコードを実行すると結果は変換する.

.. _code-block: hs

    > let x = 42
    > runQ [| x |]
    VarE Ghci4.x

この `Ghci4.x` は変数と同様の名前であり, バインドされ, 捕捉できない.
クオーとされたHaskellコードは, 同じmodule/scope/contextであるような, 同様のASTを作成する.

.. _code-block: hs

    > let withX = it -- 'it' は最後に評価された結果を表している.
                     -- この場合, VarE Ghci4.x
    > let x = 99 in $(return withX) -- 'x'へのバインドに影響を与えない
                                    -- これは名前が捕捉されないからである

クオートは現在のスコープを全探索するが, 新しい名前がここで作成されるというわけではない.

.. _code-block: hs

    > runQ [| \x -> x + 1 |]
    LamE [VarP x_4] (InfixE (Just (VarE x_4)) (VarE GHC.Num.+) (Just (LitE (Integer 1))))

この, `x_4` は自動的に生成される. これはmyFuncの最初の実装で, `newName :: String -> Q Name` 関数で導入したものと同じ種類の名前である. これは新しく, 捕捉されない.

捕捉可能な名前をつくる方法は, `mkName :: String -> Name` 関数を使用することである.

.. _code-block: hs

    > runQ [| $(varE (mkName "x")) + 1 |]
    InfixE (Just (VarE x)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))
    > let xPlus1 = it
    > let x = 99 in $(return xPlus1) -- 影響を及ぼす
    100

`Language.Haskell.TH.Lib` モジュールは, QモナドのAST値を受け取り返却するヘルパー関数が含まれている. これらのヘルパーは引用やスプライシングでうまく構成されているため, 短いコードを生成することがある. ここでは, `varE :: Name -> Q Exp` を, `VarE :: Name -> Exp` の代わりに利用している.

捕捉可能な名前をつくるもう一つの方法は, アンバインドな名前を利用することである.

.. _code-block: hs

    > withZ <- runQ [| z + 1 |]
    > withZ
    InfixE (Just (UnboundVarE z)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))
    > let z = 100 in $(return withZ)
    101

しかしアプローチはかなり壊れやすいようである. (後で同じモジュール内のどこかでzを定義するとどうなるだろうか...?)

捕捉可能な名前は時々便利である. 例えば, hamlet template systemは, #{name} というシンタックスを使用してテンプレートの値を参照できる. そのテンプレートは, そのような名前がキャプチャ可能な名前として出てくるため, バインド刷ることができるHaskellコードを生成する. その結果, テンプレートが使用されているコンテキストでバインドされた値にテンプレートでアクセスすることができる. 超クール!!

Retrieving information about things
=====================================

名前に関して少し見てきたので, 名前のついたものから情報を探す方法を学んでいこう.
これを可能にする, いわゆる「reifying」関数がかなりある.

* `reify :: Name -> Q Info` : 最も一般的に利用する. `Info` という一般的情報を探すもの.

* `extsEnabled :: Q [Extension]` : splicingされている領域で有効になっている言語拡張のリストを返す.

* `isExtEnabled :: Extension -> Q Bool` : 特定の言語拡張が有効になっているかどうかチェックする.

* `reifyInstances :: Name -> [Type]` : 型 `[Type]` の `Name(型クラス名)` の可視インスタンスのリストを返す

* その他に, `reifyFixity`, `reifyRoles`, `reifyAnnotations`, `reifyConStrictness` など.

名前が使われる場合(スコープ)は2つある?
メタプログラムを記述するときのスコープと, メタプログラムを実行するときのスコープである.
これらのスコープは独立している. メタプログラムのスコープで名前の探索を行っても, それは実行時のスコープではない. 実行時のスコープ内で何かしらにアクセスすることを考えた時, 以下の2つのことを行う

* `makeLenses` のときの様に, `Name` を引数として受け取る. この場合, `Name` をsplicing領域(スコープ)に構築し, 名前付けを行う.

* splicing領域の名前を検索するときに, `loolupTypeName` と `lookupValueName` 関数を利用することができる.

これらの関数のシグネチャは以下

`lookupTypeName :: String -> Q (Maybe Name)`
`lookupValueName :: String -> Q (Maybe Name)`

`Name` それ自身はコンテキストに依存する形で意味を変えることはできない. もし, `Name` を得たら, それは常にひとつ特定のものを指す. 従って, `lookupValueName` と `lookupTypeName` はStringを取り, `Name` を返すのが理にかなっている

以降, reifying関数をより実際的に利用してみる.

Example 1: instance generation
================================

これまで見た全てのツールがどの様に関連しているかを示すため, この例は少し工夫されている. それゆえこのコードを読んで「コードの壁」を感じる必要はない

我々は今, ある型にいくつ非ボトム値があるのかを知りたいとする.
まず, THを使わずにこう書いていけるだろう.

.. code-block:: hs

    class Countable a where
        count :: Proxy a -> Integer

型クラスaのメソッドがある型に対する「connnection」を持つことを担保するため, `Proxy` は必要である(つまり, aはcountをシグネチャに持つ必要がある).
型aの特定の値などに興味は無いが, aが `Proxy a` によってバイパスされることだけは明確にしておく.

さて, インスタンスを書くにはどうするだろうか. 既にこの問題を解決しているEnum, Bounded型のクラスを活用できるように見えなくもないが, これらは限られたタイプセットのみで有効である. 型が, EnumとBoundedの両方のインスタンスである場合, countは次のように定義できる

.. code-block:: hs

    instance (Enum a, Bounded a) => Countable a where
        count Proxy = fromIntegral $
            1 + fromEnum (maxBOund :: a) - fromEnum (minBound :: a)




.. ====================
.. リンク
.. ====================

.. _Declaration: https://hackage.haskell.org/package/template-haskell-2.13.0.0/docs/Language-Haskell-TH.html#t:Dec
.. _Expression: https://hackage.haskell.org/package/template-haskell-2.13.0.0/docs/Language-Haskell-TH.html#t:Exp
.. _Type: https://hackage.haskell.org/package/template-haskell-2.13.0.0/docs/Language-Haskell-TH.html#t:Type
.. _Pattern: https://hackage.haskell.org/package/template-haskell-2.13.0.0/docs/Language-Haskell-TH.html#t:Pat

