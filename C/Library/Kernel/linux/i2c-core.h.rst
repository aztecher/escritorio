=========================
i2c-core.h
=========================

'linux/i2c-core.h' の情報を記述する.


Functions
===============

i2c_register_driver
----------------------

::

    int i2c_register_driver(
        struct module *owner,
        struct i2c_driver *driver);

詳しくは見ていないが, I2Cドライバの登録を行う関数だろう.

i2c_del_driver
-----------------

::

    void i2c_del_driver(struct i2c_driver *driver)

I2Cドライバの登録を解除する.
第一引数に解除するドライバ構造体を与える.

i2c_smbus_read_byte_data
--------------------------

::

    s32 i2c_smbus_read_byte_data(
        const struct i2c_client *client,
        u8 command)

SMBusの "read byte" プロトコルで, 認識されたI2C機器と通信をする. 第一引数には `struct i2c_clinet` を私, その中にI2Cバス情報(使用するI2C Adapter)やスレーブアドレスを格納する. これらの情報はI2Cを認識した際のハンドラ関数の引数としてもらえる. 第二引数には, 読み込む番地のアドレスを与える. これで接続したI2C機器の特定のアドレス値を呼んでくることができる.
