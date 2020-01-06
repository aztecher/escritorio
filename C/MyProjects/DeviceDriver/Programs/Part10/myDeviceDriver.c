/*
 * I2C Device Driver
 *
 * 本デバドラは
 *
 * I2C機器は通常, 自動で認識されないされないため手動で教える.
 *
 * Date :  2018.08.24
 * @author Mikiya Michishita
 */

#include <linux/module.h>
#include <linux/i2c.h>

// Device Information
MODULE_LICENSE("Dual BSD/GPL");
#define DRIVER_NAME "MyDevice"

// このデバイスドライバで取り扱うデバイスを識別するテーブルを登録する.
// 重要なのは最初のnameフィールド. これでデバイス名を決める.
// 後ろはこのドライバで自由に使えるデータ
static struct i2c_device_id mydevice_i2c_idtable[] = {
  {"MyI2CDevice", 0},
  {}
};
MODULE_DEVICE_TABLE(i2c, mydevice_i2c_idtable);

// static int mydevice_i2c_probe(struct i2c_client *client, const struct i2c_device_id *id)
// {
//   printk("i2c_lcd_probe\n");
//   printk("id.name = %s, id.driver_data = %ld\n", id->name, id->driver_data);
//   printk("slave address = 0x%02X\n", client->addr);
// 
//   // 通常はここで, このデバドラでサポートしているデバイスかチェックする.
// 
//   int version;
//   // I2C接続されている, L3GD20のWHO_AM_I の呼び出し
//   version = i2c_smbus_read_byte_data(client, 0x0f);
//   printk("id = 0x%02X\n", version);
// 
//   return 0;
// }
// 
// static int mydevice_i2c_remove(struct i2c_client *client)
// {
//   printk("mydevice_i2c_remove\n");
//   return 0;
// }


static ssize_t get_version(struct device *dev, struct device_attribute *dev_attr, char *buf)
{
  printk("get_version\n");
  struct i2c_client *client = to_i2c_client(dev);

  int version;
  version = i2c_smbus_read_byte_data(client, 0x0f);
  return sprintf(buf, "id=0x%02X\n", version);
}
static DEVICE_ATTR(version, S_IRUGO, get_version, NULL);

static int mydevice_i2c_probe(struct i2c_client *client, const struct i2c_device_id *id)
{
  printk("mydevice_i2c_probe\n");
  printk("id.name = %s, id.driver_data = %d\n", id->name, (int)(id->driver_data));
  printk("slave address = 0x%02X\n", client->addr);

  // 通常はここで, このデバドラでサポートしているデバイスかどうかチェックする
  // このデバドラの属性読み書き用のsysfsファイルを作成.
  device_create_file(&client->dev, &dev_attr_version);
  return 0;
}

static int mydevice_i2c_remove(struct i2c_client *client)
{
  printk("mydevice_i2c_remove\n");
  device_remove_file(&client->dev, &dev_attr_version);
  return 0;
}


static struct i2c_driver mydevice_driver = {
  .driver = {
    .name = DRIVER_NAME,
    .owner = THIS_MODULE,
  },
  .id_table = mydevice_i2c_idtable, // このデバドラがサポートするI2Cデバイス
  .probe = mydevice_i2c_probe, // 対象とするI2Cデバイスが認識された時に呼ばれる処理
  .remove = mydevice_i2c_remove, // 対象とするI2Cデバイスが取り外されたときに呼ばれる処理
};

// // ロード(insmod)時に呼ばれる関数
// static int mydevice_init(void)
// {
//   printk("mydevice_init\n");
//
//   // 本デバイスドライバを, I2Cバスを使用するデバドラとして登録する.
//   i2c_add_driver(&mydevice_driver);
//   return 0;
// }

// 機器の認識を自動化するように mydevice_init を修正
static struct i2c_client *i2c_clie = NULL;
static int mydevice_init(void)
{
  printk("mydevice_init\n");

  // 本デバイスドライバを, I2Cバスを使用するデバドラとして登録する.
  i2c_add_driver(&mydevice_driver);

  // 動的にデバイスを作る.
  // https://www.kernel.org/doc/Documentation/i2c/instantiating-devices
  // I2C1に接続された, "MyI2CDevice" という名前で,
  // スレーブアドレスが 0x6b のデバイスを作る.
  struct i2c_adapter *i2c_adap;
  i2c_adap = i2c_get_adapter(1);
  struct i2c_board_info i2c_board_info = {
    I2C_BOARD_INFO("MyI2CDevice", 0x6b)
  };
  i2c_clie = i2c_new_device(i2c_adap, &i2c_board_info);
  i2c_put_adapter(i2c_adap);

  return 0;
}

// // アンロード(rmmod)時に呼ばれる関数
// static void mydevice_exit(void)
// {
//   printk("mydevice_exit\n");
//   i2c_del_driver(&mydevice_driver);
// }

// 機器認識を自動化したため, アンロード時の関数にも少し手続きが必要
static void mydevice_exit(void)
{
  printk("mydevice_exit\n");

  i2c_del_driver(&mydevice_driver);
  if (i2c_clie) i2c_unregister_device(i2c_clie);
}

module_init(mydevice_init);
module_exit(mydevice_exit);
