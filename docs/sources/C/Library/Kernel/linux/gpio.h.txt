=========================
gpio.h
=========================

'linux/gpio.h' の情報を記述する.

Functions
============

gpio_direction_output
----------------------

::

    int gpio_direction_output(unsigned gpio, int value)

深くは追えなかったが, GPIOを出力に設定する関数.
第一引数にGPIOのピン番号, 第二引数には初期出力値(0=low, 1=high)を設定する.

gpio_direction_input
----------------------

::

    int gpio_direction_input(unsigned gpio)

深くは追えなかったが, GPIOを入力に設定する関数.
第一引数にGPIOのピン番号を渡す.

gpio_set_value
----------------------

::

    void gpio_set_value(unsigned gpio, int value)

深くは追えなかったが, GPIOに出力する値を設定する.
第一引数にGPIOのピン番号, 第二引数に出力する値(0=low, 1=high)

gpio_to_irq
----------------------

::

    int gpio_to_irq(unsigned gpio)

深くは追えなかったが, GPIOの指定したピンの割り込み番号を取得する.
第一引数にGPIOのピン番号を渡す.

