=========================
device.h
=========================

'linux/device.h' の情報を記述する.

Structures
============

class
--------

::

    struct class {
        ...
        }

詳しくは省略するが, デバイスを表すクラス.
(なんでclassなんていう紛らわしい名前にしたのか...)

device_driver
-----------------

::

    struct device_driver {
        -- @name : デバドラの名前
        -- @owner : オーナーモジュール
        -- @of_match_table : open firmware テーブル
        const char *name;
        const module *owner
        const struct of_device_id *of_match_table
        ....
    }

基本的なデバイスドライバ構造体. 例えば, `i2c_device_driver` などで利用される.

Functions
============

class_create
--------------

::

    struct class *class_create(
        struct module* owner,
        const char* name);

`struct class` のオブジェクトを得るために使用する関数. 第一引数には, 基本 `THIS_MODULE` を与えるといいかもしれない(違ったら修正). 第二引数には作成するデバイスクラス名 (作成されるクラス `/sys/class/XXX` の `XXX` に対応する名前)

device_create
---------------

::

    struct device *device_create(
        struct class *class,
        struct device *parent,
        dev_t devt,
        const char* fmt,
        ...);

deviceを作成し, それをsysfsに登録する.
(詳しくは現状イマイチよく分からなかったため省略)
この関数は, キャラクタデバイスクラスで使用できる. sysfsには, 指定したクラスに登録されたstructデバイスが作成される.

device_destroy
---------------

::

    void device_destroy(
        struct class *class,
        dev_t devt);

device_createで作成されたデバイスを取り除く.

device_create_file
---------------------

::

    int device_create_file(
        struct device* dev,
        struct device_attribute* attr);

デバイス用のsysfs属性ファイルを作成する.
第一引数にデバイスを, 第二引数に登録するデバイス属性ディスクリプタを与える.
第二引数のデバイス属性ディスクリプタは, `DEVICE_ATTR` の第一引数に設定した名前に, `dev_attr_` というプレフィックスを与えたものになる.

device_remove_file
---------------------

::

    void device_remove_file(
        struct device* dev,
        struct device_attribute* attr);

sysfs属性ファイルを削除する.
第一引数にデバイスを, 第二引数に登録したデバイス属性ディスクリプタを与える.
`device_create_file` 関数で作成したsysfsの削除関数であり, この関数の引数は `device_create_file` 関数を呼び出した際の引数と一致させる.


devm_kzalloc
---------------

::

    void *devm_kzalloc(
        struct device* dev,
        size_t size,
        gfp_t gfp);

リソース管理されたkzalloc (ドライバ画でタッチされたら自動でメモリを解放する) (kzallocはメモリアロケーション+ゼロ埋め)
第一引数に, `device` 構造体が格納されたメモリ, 第二引数にサイズ, 第三引数にgfpフラグを与える.


Macro
=========

DEVICE_ATTR
---------------

::

    #define DEVICE_ATTR(_name, _mode, _show, _store) \
        struct device_attribute dev_attr_##_name = 
            __ATTR(_name, _mode, _show, _store)

`__ATTR` は `sysfs.h` で定義されている(ここではこれ以上追わない)
`DEVICE_ATTR` を実行すると, 結果として第一引数に `dev_attr` という名前がついた `device_attribute` 型の構造体が宣言される.

第一引数は名前(作成される構造体の名前のベースになる), 第二引数はsysfsのモード, 第三引数はread用ハンドラ, 第四引数はwrite用のハンドラとなる.
