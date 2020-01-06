=========================
mod_devicetable.h
=========================

'linux/mod_devicetable.h' の情報を記述する.

Structures
============

i2c_device_id
--------------

::

    struct i2c_device_id {
        char name[I2C_NAME_SIZE];
        kernel_ulong_t driver_data; // data private to the driver
    };

作成するI2Cデバイスドライバに登録するI2Cの情報用の構造体.  第一引数はデバイス名, 第二引数にはこのデバドラ内で使うプライベートデータが格納される. 大引数は通常, 識別用の数字を入れる.


of_device_id
----------------

::

    struct of_device_id {
        char name[32];
        char type[32];
        char compatible[128];
        const void *data;
    };

デバイスツリー内で特定のデバイスについて記述されたドライバを探すために使われる情報の構造体. 恐らく, デバイスツリーに登録してビルドしたデバイス(これは*.dtsを編集してビルドするプログラムとは別の処理で, I2C機器等をデバイスとして認識させる周りの処理)に対応するデバイスドライバを, (恐らくカーネルが)検索するために利用する情報をまとめた構造体のことだろう. この構造体に, `*dts` ファイルに記述した情報と類似の情報を登録した構造体(`hogehoge`)を作成, `MODULE_DEVICE_TABLE(of, hogehoge)` として登録する.
