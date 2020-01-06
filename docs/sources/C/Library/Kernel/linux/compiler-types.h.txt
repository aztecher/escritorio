=========================
compiler-types.h
=========================

'linux/compiler-types.h' の情報を記述する.

Macro
============

__user
--------

よくわからないけど以下のような定義になっていた.
(今はこれ以上突っ込まない)

::

    #define __user  __attribute__((noderef, address_space(1)))


