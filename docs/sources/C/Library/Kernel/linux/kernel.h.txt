=========================
kernel.h
=========================

'linux/kernel.h' の情報を記述する.

Functions
============

printk
--------

printk関数の出力先はカーネルバッファ.
カーネルバッファは, `dmesg` コマンドで参照できる.
また, syslogd や klogd がカーネルバッファをsyslog(/var/log/message)に定期的に出力紙ているため, syslogからも確認できる.

:: 

    int printk ( const char *fmt, … )

	書式指定(fmt)の先頭に「<番号>」を付けると、メッセージのログレベル(優先度)を指定できる。
	「include/linux/kernel.h」にマクロ定義。
	・KERN_EMERG
	    システムは使用できない
	・KERN_ALERT
        即時にアクションを取る必要がある
	・KERN_CRIT
	    かなり憂慮すべき状態
	・KERN_ERR
	    エラー状態
	・KERN_WARNING
	    警告状態
	・KERN_NOTICE
	    異常ではないが、重要な状態
	・KERN_INFO
	    情報
	・KERN_DEBUG
	    デバッグ・メッセージ

Macros
==========

container_of
-----------------

::

    -- cast a member of a structure out to the containing structure
    -- @ptr:    the pointer to the member
    -- @type:   the type of the container struct this is embedded in.
    -- @member: the name of the member within the struct.

    #define container_of(ptr, type, member) ({
        const typeof( ((type *)0)->member ) *__mptr = (ptr);
        (type *)( (char *)__mptr - offsetof(type, member) );})

正直頭の痛いマクロだが, これが大変便利で, 実行内容としては「変数が入っている構造体へのポインタをコンパイル時に計算するもの」らしい.
