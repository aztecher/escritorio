=========================
module.h
=========================

'linux/module.h' の情報を記述する.

Structures
=============

module
--------

::

    struct module {
        ....
        }

よくわからんけど, `THIS_MODULE` とかはこれのマクロっぽい?

Functions
============

module_init
------------------

::

    #define module_init(x) __initcall(x)

module_init() はドライバイニシャライゼーションのエントリーポイント.
引数xは, カーネルがブートするタイミング, もしくはモジュールが挿入されるタイミングで動作させる関数

module_exit
------------------

::

    #define module_exit(x) __exitcall(x)

module_exit() はドライバエグジットのエントリーポイント
引数xは, ドライバが取り外されるタイミングで動作させる関数


Macros
===========

MODULE_LICENSE
---------------

Linux2.4.10以降に追加された, 自身のmoduleに適用されているライセンスを示すためのマクロ


MODULE_DEVICE_TABLE
-----------------------

このマクロは全てのUSB, PCIドライバで利用されるもので, 特定のドライバがサポートするデバイスについて記述する. `MODULE_DEVICE_TABLE(usb, puzzle_id_table);` とすると, `__module_usb_device_size` と `__module_usb_device_table` という変数が作成される. 前者は `usb_id` 構造体のサイズ値を含んでおり, 後者は `puzzle_id_table` 構造体を指している(アドレスかなにかか?).

正直いまいち解説が見つからないのでこれ以上はドキュメントを呼んだほうが良さそう.
