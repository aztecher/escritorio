=========================
interrupt.h
=========================

'linux/interrupt.h' の情報を記述する.

Structures
===========

irqreturn_t
----------------

割り込みハンドラの返り値になっている型.
細かくは未調査だが, `IRQ_NONE` を返した場合, 「割り込みは自分自身ではなかった」という意, `IRQ_HANDLED` は「割り込みをきちんと処理した」という意.

全割り込みハンドラが `IRQ_NONE` を返せば, なんらかのトラブルを意味する. 複数の割り込みハンドラが `IRQ_HANDLED` を返すと正常.

irq_handler_t
-----------------

細かくは未調査だが, 割り込みの基本的な型だとおもう.

Functions
============

request_irq
-----------------

::

    int request_irq(
        unsigned int irq,
        irq_handler_t handler,
        unsigned long irqflags,
        const char *devname,
        void *dev_id);

IRQの使用宣言. IRQと割り込みハンドラをカーネルに登録する. IRQの使用状況は `/proc/interrupt` で確認できる.
第一引数にirq番号(gpio_to_irqの返り値など)を, 第二引数にIRQが発生したときに呼び出す関数(ハンドラ)を, 第三引数には割り込みタイプをフラグで指定し, 第四引数にはasciiの名前(`/proc/interrupts` などで割り込み状況を確認するときなんかに使われるやつだろう)を, 第五引数は割り込みが複数デバイスで共有されるときに(`IRQ_SHARED`), どのデバイスかを区別するために使われるデバイス名. 普通は, 構造体へのポインタ. 割り込みハンドラが呼ばれたときにもこの値が引数で渡される. `free_irq()` 割り込みハンドラを削除するときには, この値がcookieとして働く.

フラグをいくつか記述しておく
* `IRQ_SHARED` : 基本的に一つのIRQには一つのハンドラだが, これをつけて登録すると, 一つのIRQに複数のハンドラが登録できる.
* `IRQ_TRIGGER_RISING` : 立ち上がりを検出し割り込みを発生させる.
* `IRQ_TRIGGER_FAILING` : 立ち下がりを検出し割り込みを発生させる.

(これ以上は必要になったりしたら書く)

以前, SA_*** という名前だったらしいがこれが廃止になって上記のようなフラグになったらしい.


free_irq
------------

::

    void free_irq(unsigned int irq, void *dev_id);

`request_irq` で登録された割り込みハンドラを解放する.
第一引数にirq番号, 第二引数にrequest_irq()の第五引数で指定したデバイス名.
