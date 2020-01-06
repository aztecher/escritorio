=========================
io.h
=========================

'linux/io.h' の情報を記述する.

(io.h関係はアーキテクチャ依存の部分がある? この記事を書くはじめのきっかけはRaspi用のデバドラ開発であったが, そこではラズパイのカーネルヘッダのうちの, `arch/arm/include/asm/io.h` にかかれているもの(多分)であった.)

現状はいまいちつかめていないが, おそらくアーキテクチャ依存とはいえ機能は同じであろうと思う.

Functions
============


.. _tag_ioremap:
ioremap
--------------

::

    void *ioremap(
        unsigned long phys_addr,
        unsigned long size);

I/Oメモリをカーネルアドレス空間(仮想アドレス)にリマップする.
第一引数で物理アドレス(I/Oメモリのアドレス)の開始を指定し, 第二引数でマップする物理アドレスのサイズを指定する.

返り値としては, マッピングされた領域(仮想アドレス)の開始アドレスを得る.

ioremap_nocache
--------------------

::

    void __ioremap_nocache(
        resource_size_t offset,
        unsigned long size);

キャッシュなしで `ioremap` を行う.
詳しくは :ref:`tag_ioremap<ioremap>` を参照.

iounmap
-----------

::

    void iounmap (void *addr)

I/Oメモリをカーネルアドレス空間(仮想アドレス)からアンマップする.
引数には, マップされたI/Oアドレスの先頭アドレスを渡す.
