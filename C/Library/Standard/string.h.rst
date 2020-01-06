=========================
string.h
=========================

'string.h' の情報を記述する.

Functions
============

strlen
--------

::

    size_t strlen(const char *str);

文字列strからNULL終端文字列(\0)の直前までの文字数を求める.
戻り値は, 文字列strの長さを返す.

strlcat
--------------

::

    size_t strlcat(
        char *dst,
        const char *src,
        size_t size);

2つの文字列を連結する.
この関数は最大 `size-strlen(dst)-1` バイトをNUL終端文字列srcから
dstの末尾に追加する.
