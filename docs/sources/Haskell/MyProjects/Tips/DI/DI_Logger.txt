==============
DI Logger
==============

例えば, Loggerを作る例を考える.

設計
=====

.. code-block:: hs

    class Logger a where
      writeLog :: a -> String -> IO ()

ロガーは文字列を受け取って何かをするというインターフェースを実装した型のことであろう. ここで `a` に, 具体的なLoggerの型が挿入される.

.. code-block:: hs

    data SomeLogger = forall a. Logger a => SomeLogger a

    instance Logger SomeLogger where
      writeLog (SomeLogger i) = writeLog i

`SomeLogger` という型を用意しておく. SomeLoggerはロガーのinstanceが閉じ込められている. また, SomeLoggerは自明にLoggerとしての機能を与えることができるので, それも与えておく.

.. code-block:: hs

    type UserLoggger = Given SomeLogger

    useLogger :: UseLogger => SomeLogger
    useLogger = given

`reflection` のAPIを専用関数としてラップしたものを用意しておく. この辺はこのみ(巷のDIコンテナのノリに合わせている)

実装
======

では実装してみる.

.. code-block:: hs

    data StdoutLoggerImpl = StdoutLoggerImpl

    instance Logger StdoutLoggerImpl where
     writeLog _ str = putStr str

    newLogger :: SomeLogger
    newLogger = SomeLogger StdoutLoggerImpl

せやなという感じ.

Usage
=======

ロガーを使いたいときは次のようにすればいい.

something :: UserLogger => ...
something = do
  writeLog userLogger "write to log!"

`UserLogger =>` のなかでは, `userLogger` を使うことができてロガーを使うことができる. ここではロガーの具体的な型は言及しなくても良いところが大事.

注入
=======

注入したいときは, `give StdoutLoggerImpl.newLogger $ ...` ってやる.
アプリケーションの一番外側のレイヤでやればいい.

この手法について
==================

多分, Someﾅﾝﾄｶの型を作るところとかがボイラープレートだらけなのでそこらへんはもう少し色々提供してあげたい. 例えば, 上のnewLoggerで間違えて `StdoutLoggerImpl` を提供した時, これをgiveしようとするとエラーになるがそういうときのエラーメッセージはあまり適切ではないと思う.

ただ実際にやっていることは薄いのでフレームワークってほど難しくもなく, 使うのは簡単なはず.

