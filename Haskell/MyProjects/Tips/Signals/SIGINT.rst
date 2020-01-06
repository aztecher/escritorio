============
SIGINT
============

Simple Example
================

例として, `Ctrl-C` が押されたらクロージング処理を伴って終了するプログラムを書きながら確認していく.
メインスレッドのスレッドIDを取得しておき, それをSIGINTで(下記例ではハンドラを呼び出し)停止させる際にクロージング処理をする.

.. code-block:: hs

    import Control.Concurrent
    import System.Posix.Signals

    main :: IO ()
    main = do
        tid <- myThreadId
        let handler = do
            putStrLn "goodbye!"
            killThread tid
        installHandler keyboardSignal (Catch handler) Nothing

        let loop n = do
            putStr $ show n ++ ", "
            threadDelay 1000000
            loop (n + 1)
        loop 0

ちゃんと goodbye と出力され終了した!
`killThread` は例外を伴って終了するため, `Main.hs: thread killed` というメッセージが出てしまっている.
もし気になるなら, 例外を握りつぶすか `MVar` を使って終了を監視する仕組みをつくるといいだろう.

.. code-block:: hs

    -- Signalを制御する関数 - installHandler
    -- Arg1 : Signal : 制御するSignal
    -- Arg2 : Handler : Signalが投げられたときの処理
    -- Arg3 : Maybe SignalSet : 処理時にブロックする他のSignalを指定する
    installHandler :: Signal -> Handler -> Maybe SignalSet -> IO Handler

    -- Signalの値は予め, System.Posix.Signals` に用意されている.
    SIGINT : keyboardSignals
    SIGTSTP : keyboardStop
    SIGQUIT : SIGQUIT
    SIGPIPE : openEndedPipe

`Handler` は `Default` にすれば標準の動作を, `Ignore` にすれば何もしなくなり, `Catch (IO ())` で処理を記述すれば Signal が投げられたときの処理を記述することができる.
上の例では, `handler` という関数にメッセージの表示とメインスレッド停止の処理を書いて, `Handler` として渡していた.

Handling cleanly
==================

こちらではもう少しきれいにやる


