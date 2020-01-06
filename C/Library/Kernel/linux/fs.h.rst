=========================
fs.h
=========================

'linux/fs.h' の情報を記述する.

Structures
============

file_operations
------------------

構造体には関数ポインタがつらつらと並んでいる.
これがエントリポイント.
定義されている内容をいくつかピックアップしてみる.

::

    struct file_operations {
        ...
        ssize (*read) (struct file *, char __user *, size_t, loff_t *)
        ssize (*write) (struct file *, const char __user *, size_t, loff_t *)
        int (*open) (struct inode *, struct file *)
        int (*release) (struct inode *, struct file *)
        long (*unlocked_ioctl) (struct file *, unsigned int, unsigned long)
        long (*compat_ioctl) (struct file *, unsigned int, unsigned long)
        ...
    }


inode
------

構造体として定義されていた.
(詳しくは読んでいない)
VFSとかでも使うファイルシステム系の構造体定義っぽい?


file
-----

構造体として定義されていた.
(詳しくは読んでいない)
VFSとかでも使うファイルシステム系の構造体定義っぽい?

Functions
============

register_chrdev
------------------

::

    int register_chrdev(
        unsigned int major,
        const char *name,
        const struct file_operations *fops);


メジャー番号をドライバーエントリーポイントセット(キャラクターデバイス)に関連付ける.
第二引数のnameはデバイスのショートネームで, /proc/devices に表示される.
また, unregister_chrdev を呼び出し, メジャー番号(ドライバ)を解放する際のマッチングにも必要である.

unregister_chrdev
-------------------

::

    void unregister_chrdev(
        unsigned int major,
        const char *name);

メジャー番号(ドライバ)を解放する.
そして, 通常 `module_cleanup` 関数を利用してカーネルからドライバを取り外す.


alloc_chrdev_region
----------------------

::

    int alloc_chrdev_region(
        dev_t *dev,
        unsigned baseminor,
        unsigned count,
        const char *name);

ある範囲のキャラクター型デバイス番号を登録する.
メジャー番号は動的に選択され,
devの最初のマイナー番号とともに返される.
第二引数は, マイナー番号のレンジのはじめの値.
第三引数は, 要求するマイナー番号の数.
第四引数は, 関連付けら得れるデバイスやドライバの名前.

unregister_chrdev_region
--------------------------

::

    void unregister_chrdev_region(
        dev_t from,
        unsigned count);

第一引数のfromから, 第二引数の数字までの範囲のデバイスナンバーの登録を解除する.
