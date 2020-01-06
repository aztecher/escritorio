=========================
cdev.h
=========================

'linux/cdev.h' の情報を記述する.

Structures
============

cdev
--------

キャラクター型デバイス(char device)を表現している構造体.
inode構造体の要素の一つにもなっている.


Functions
============

cdev_init
-----------

::

    void cdev_init(
        struct cdev *cdev,
        const struct file_operations *fops);

cdev構造体(キャラクター型デバイス)を初期化する関数
第一引数に初期化したいcdev構造体を, 第二引数にこのデバイスの `file_operations` 構造体を渡す.
cdev_add関数を呼び出してシステムにデバイスを登録する前の準備として実行する.

cdev_add
----------

::

    int cdev_add(
        struct cdev *p,
        dev_t dev,
        unsigned count);

キャラクター型デバイスをその番号と共にシステムに登録する関数.
第一引数には, (ハンドラセットなどを終えた)cdev構造体を与え,
第二引数には, dev_t型のキャラクタデバイス番号(最初のデバイス番号)
第三引数には, このデバイスに対応する連続するマイナー番号の数.

わかりやすいようにざっくりいうと, countが1でマイナー番号の開始番号が0の場合,
/dev/hogehoge0
が作られる.
countが2でマイナー番号の開始番号が1の場合,
/dev/hogehoge0
/dev/hogehoge1
が作られる.

cdev_del
----------

::

    void cdev_del(struct cdev *p)

システムからキャラクター型デバイスを取り除く関数.
取り除きたい cdev構造体を渡すと, システムからcdevが取り除かれる.
