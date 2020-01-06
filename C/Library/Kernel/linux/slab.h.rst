=========================
slab.h
=========================

'linux/slab.h' の情報を記述する.

Functions
============

kmalloc
--------------

::

    void *kmalloc(
        size_t size,
        gfp_t flags);

    flags:
    GFP_USER   - Allocate memory on behalf of user. May sleep.
    GFP_KERNEL - Allocate normal kernel ram. Mey sleep.

メモリーアロケーション(メモリ割り当て)
カーネルのメモリ割り当てを行う.
第一引数で確保するメモリサイズを指定し,
第二引数でフラグを指定する.
第二引数のフラグは様々あるが, カーネル上のメモリ確保には `GFP_KERNEL` フラグを利用すれば良さそう.

kfree
--------------

::

    void kfree(const void *objp);

既に割り当てられたメモリを解放する.
kmallocで割り当てられたメモリを開放するために利用する.
引数に, kmallocで返されるポインタ(割り当てられたメモリのアドレス)を与える.
