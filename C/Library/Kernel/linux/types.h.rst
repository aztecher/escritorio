=========================
types.h
=========================

'linux/types.h' の情報を記述する.

types
============

size_t
--------

よくわからないけど以下のような定義になっていた.
(今はこれ以上突っ込まない)

::

    typedef __kernel_size_t size_t;

ssize_t
---------

よくわからないけど以下のような定義になっていた.
(今はこれ以上突っ込まない)

::

    typedef __kernel_ssize_t ssize_t;


loff_t
---------

よくわからないけど以下のような定義になっていた.
(今はこれ以上突っ込まない)

::

    typedef __kernel_loff_t loff_t;


dev_t
-------

linuxで, デバイス番号を取り扱う型.
dev_t型変数からメジャー番号を取得する場合, MAJOR(dev_t)を使い,
dev_t型変数からマイナー番号を取得する場合, MINOR(dev_t)を使う.
整数型変数のメジャー番号とマイナー番号からdev_t型のデバイス番号を作成するには, MKDEV()を使う.

::

    typedef __kernel_dev_t dev_t;


