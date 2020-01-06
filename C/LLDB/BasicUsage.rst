==============
Basic Usage
==============

LLDBに関する基本的な使い方や, デバッグの際のテンプレート処理などを備忘録として記述しておく.

Basic Usage
=============

LLDBのコマンドはGDBより構造的な構文になっている. コマンドは, 全て以下のようなフォームになっている.

.. code-block:: sh

    <noun> <verb> [-options [option-value]] [argument [argument...]]

コマンド, オプション, 引数はスペースで区切られる. スペースが必要なものにはダブルクオーテーションが使用できる.

コマンドに対するオプションはどこでも指定可能. ただし, 引数(argument)が, '-' から始まる場合, LLDBにオプションの終了位置を '--' で教えてやる必要がある. 例えば, ``process launch`` というコマンドを ``--stop-at-entry`` オプションと一緒にLLDBに与える時, かつコマンドの引数を ``-program_arg value`` で与える時, コマンドは以下のようになる.

.. code-block:: sh

    > (lldb) process launch --stop-at-entry -- -program_arg value


基本的なコマンドの一覧
-------------------------

``help`` コマンドで表示されるが, 日本語は以下のページを参考にするといいだろう.

* lldbで使えるコマンド一覧_
* gdb-lldb対応表_

プログラムのロード
--------------------

基本的には, 直接引数として指定するか, fileコマンドで取り込む.

.. code-block:: sh

    $ lldb optical_simulator

.. code-block:: sh

    $ lldb
    > (lldb) file optical_simulator


プログラムの実行
------------------

``run`` もしくは, ``r`` で実行.

.. code-block:: sh
    > (lldb) run

Breakpoint
-------------

``breakpoint`` もしくは, ``br`` を使い, 設定する場合は, ``set`` もしくは ``s`` を利用する. その他オプションに関しては, 以下に例示しながら記述する.

.. code-block:: sh

    # ファイル foo.c の12行目にBreakpointを設定
    > (lldb) breakpoint set --file foo.c --line 12
    > (lldb) breakpoint set -f foo.c -l 12

    # 関数 foo にBreakpointを設定
    > (lldb) breakpoint set --name foo
    > (lldb) breakpoint set -n foo
    # --nameオプションは1 breakpointコマンドに対して複数指定可
    > (lldb) breakpoint set --name foo --name bar

    # メソッド名でも指定可能
    > (lldb) breakpoint set --method foo
    > (lldb) breakpoint set -M foo

    # --shlibオプションで, Breakpointを実行可能ファイルに限定できる.
    > (lldb) breakpoint set --shlib foo.dylib --name foo
    > (lldb) breakpoint set -s foo.dylib -n foo
    # --nameオプションのように, 複数指定可能

    # Breakpointの一覧を表示
    > (lldb) breakpoint list

Watchpoint
-------------

``watchpoint`` もしくは ``watch`` を利用する. ``set`` などは, ``breakpoint`` の場合と同じ.

.. code-block:: sh

    # 変数 int i をwatchpointに設定する.
    > (lldb) watchpoint set variable i

    # watchpointの確認
    > (lldb) watchpoint list


変数出力
---------

``po`` コマンド(Print Object)を使って変数を出力できる.

.. code-block:: sh

    > (lldb) po name
    "edo"

構造体出力
-----------

``p`` コマンドを使うと構造体名が出力される.
また, ``p val[0]`` と, ``[0]`` を指定することで, 構造体の中身を見ることができる.

.. code-block:: sh

    > (lldb) p res
    (addrinfo *) $12 = 0x00000001003004a0
    > (lldb) p res[0]
    (addrinfo *) $13 = {
      ai_flags = 0
      ai_family = 2
      ai_socktype = 1
      ai_protocol = 6
      ai_addrlen = 16
      ai_cannonname = 0x0000000000000000
      ai_addr = 0x0000000100300460
      ai_next = 0x0000000000000000
    }

変数の一覧を出力する
---------------------

現在breakしているフレームで見ている変数の一覧を出力できる.

.. code-block:: sh

    > (lldb) frame variable
    (int) argc = 1
    (char **) argv = 0x00007fff5fbff460
    (char *) test = 0x0000000100000f6e "This is a debug demo."
    (char *) name = 0x0000000100000f84 "edo"
    (int) age = 20

以下のようにすると, 該当の変数を出力もできる.

.. code-block:: sh

    > (lldb) frame variable *argv
    (char *) *argv = 0x00007fff5fbff5e0

    > (lldb) frame variable age
    (int) age = 20

ステップ実行
-------------

現在の一から一行分だけ処理を進める場合,  ``n`` (``next``) か, ``s`` (``step``)を使って処理を進める. ``n`` の場合, 関数はステップ・オーバーし次の行で停止する. ``s`` の場合, 関数はステップインする.

スタックトレースを出力する
----------------------------

``bt`` (thread backtrace) コマンドで出力することができる.

.. code-block:: sh

    > (lldb) bt
    ...(省略)



Tips
=======

Alias
--------

Aliasを作成することができる. 例えば, 長いコマンドの短縮形を教えて利用することができたりする. 例えば, breakpointのコマンドで, ``(lldb) breakpoint set --file foo.c --line 12`` というコマンドのエイリアスを貼る時に, ``(lldb) command alias bfl breakpoint set -f %1 -l %2`` とLLDBに教えてあげると, ``(lldb) bfl foo.c 12`` と, シェルのように引数を与えられる.

lldbは立ち上がり時に, ``~/.lldbinit`` ファイルを読みに行くため, そのファイル内にaliasコマンドを書き連ねておくと, いちいちlldb起動時にaliasを登録し直すといった面倒な作業は発生しなくなる. ユーザが定義したaliasもhelpで参照可能なので, 自分で定義したaliasの意味を忘れても安心.

コマンド補完
--------------

lldbでは, TABキーによるコマンド補完が使用可能. ソースファイル名・シンボル名・その他ファイル名を補完する. ``--file`` オプション指定地にはファイル名を, ``--shlib`` オプション指定時にはロード済みのっ共有ライブラリ名を補完する.

Help
------

lldbはコマンドヘルプが超絶充実しているため, helpコマンドを活用するといい. aproposコマンドでhelpテキストなどを検索可能.

以下のように, helpで出てくるコマンド例の引数の意味も調べられる.

.. code-block:: sh

    > (lldb) help command alias
    ... (中略)
    Syntax: command alias <alias-name> <cmd-name> [<options-for-aliased-command>]
    > (lldb) help <alias-name>
    <alias-name> -- The name of an abbreviation (alias) for a debugger command.


基本的にはこれを読めばいいだろう. というか読む癖をつけたほうがいい.



.. ==============
.. リンク一覧
.. ==============

.. _lldbで使えるコマンド一覧: https://yokaze.github.io/2018/01/06/
.. _gdb-lldb対応表: http://lldb.llvm.org/lldb-gdb.html
