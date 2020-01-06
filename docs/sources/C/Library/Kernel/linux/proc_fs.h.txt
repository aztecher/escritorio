=========================
proc_fs.h
=========================

'linux/proc_fs.h' の情報を記述する.

Structures
============

proc_dir_entry
------------------

::

    struct proc_dir_entry

定義がどこにあるかあからなかったが, とりあえずそれっぽい記述を見つけたので一応参考程度に載せておく(Understanding_Linux_Proc_File_System_).
`proc_create` 関数により作成されるっぽいが, あまり詳しくは省略する(必要になったら書く)

Functions
============

proc_create
------------------

::

    struct proc_dir_entry *proc_create(
        const char *name, umode_t mode, struct proc_dir_entry *parent,
        const struct file_operations *proc_fops)

以前は, procファイルシステムへの登録を, `proc_create_entry` 関数を利用していたが今は廃止になっている.
そのかわりの関数がこの `proc_create` 関数である.
内部的には, `proc_create_data` が呼ばれているようだが詳細は不明(とりあえず動作が分かればいいので, 必要になったら書く).
ドキュメントが見当たらないため, 若干の勘になる(見つかれば記述する)が, 第一引数はデバイス名, 第二引数ではデバイスのモードの指定, 第三引数はイマイチ不明(ルートのポインタを与えているぽいが....とりあえずNULL), 第四引数にはハンドラテーブルを渡して呼び出すと, `/proc/name(第一引数で与える名前)` のprocfsを作成する(と思う).

remove_proc_entry
--------------------

::

    void remove_proc_entry(const char *, struct proc_dir_entry *);

`proc_create` 関数で作成されたprocfsを削除する.
第一引数に作成した際に与えた名前, 第二引数に作成した際のルートのポインタを渡す.

(余談だが, `proc_remove` 関数を見つけた. こちらのほうがいいのではないか...?)

.. ===================
.. リンク
.. ===================

.. _Understanding_Linux_Proc_File_System: http://tarekamr.appspot.com/blog/unix-proc-filesystem.html
