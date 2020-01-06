=========================
i2c.h
=========================

'linux/i2c.h' の情報を記述する.

Structures
============

i2c_driver
--------------

::

    struct i2c_driver{
        ...
        -- @probe : デバイスがバインドされたときのコールバック
        -- @remove : デバイスがアンバインドされたときのコールバック
        -- @id_table : このドライバがサポートするI2Cデバイスのリスト
        -- @driver : このドライバのモジュールオーナや名前をセット

        int (*probe) (struct i2c_client *, const struct i2c_device_id *);
        int (*remove) (struct i2c_client *);
        const struct i2c_device_id *id_table;
        struct device_driver driver;
        }

i2cデバイスドライバを表す構造体
一部の抜粋に留めるが, 説明は上記コードを見ればわかるだろう.

i2c_client
------------

::

    struct i2c_client {
        ...
        -- @addr : 親のアダプタとI2Cバスコネクションをする時のアドレス
        -- @dev : スレーブのドライバモデルデバイスノード(device構造体)

        unsigned short addr;
        struct device dev;
    }

I2Cスレーブデバイスを表現する.


Functions
===============

i2c_del_driver
-----------------

::

    void i2c_del_driver(struct i2c_driver *driver)

I2Cドライバの登録を解除する.
第一引数に解除するドライバ構造体を与える.

i2c_set_clientdata
---------------------

::

    static inline void i2c_set_clientdata(struct i2c_client *dev, void *data)
    {
        dev_set_drvdata(&dev->dev, data);
    }

`struct i2c_client` に紐付いた情報を自由に保存することができる. この情報は何でもいいが, 通常は, probe時にalloc下デバドラ独自の構造体へのポインタを保持する. 取り出すときは, `i2c_get_clientdata()` を使う.

i2c_get_clientdata
---------------------

::

    static inline void *i2c_get_clientdata(const struct i2c_client *dev)
    {
        return dev_get_drvdata(&dev->dev);
    }

`i2c_set_clientdata` で紐づけた情報を取得することができる.

Macros
=======

to_i2c_client
--------------

::

    #define to_i2c_client(d) container_of(d, struct i2c_client, dev)

これは内部的に `container_of` を呼んでいるだけである. :doc:`kernel.h` に定義されているため, 詳細はそちらに任せるが, 引数dをメンバとしてもつ構造体 `i2c_client` へのポインタを, 引数d(構造体メンバへのポインタ)から得る関数.


i2c_add_driver
------------------

::

    #define i2c_add_driver(driver) i2c_register_driver(THIS_MODULE, driver)

内部的にi2c_register_driverを呼んでおり, これは :doc:`./i2c-core.h` で定義されている. 詳しくは見ていないがI2Cドライバの登録を行う関数だろう.

module_i2c_driver
--------------------

::
    #define module_i2c_driver(__i2c_driver)
        module_driver(__i2c_driver, i2c_add_driver, i2c_del_driver)

I2Cドライバを登録するヘルパーマクロ. I2C機器用のデバドラでは, ロード/アンロード時に行う処理は, 殆どの場合 `struct i2c_driver` の登録だけなので, このヘルパーマクロで簡略化できる.
