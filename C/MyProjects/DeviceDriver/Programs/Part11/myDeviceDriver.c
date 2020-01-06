/*
 * デバイスツリーに登録した
 * "mycompany,myoriginaldevice"
 * という情報に合わせて, デバドラ側でもデバイスを登録する.
 *
 * Date :  2018.08.24
 * @author Mikiya Michishita
 */

#include <linux/module.h>
#include <linux/i2c.h>
#include <linux/of_platform.h>

// Device Information
MODULE_LICENSE("Dual BSD/GPL");
#define DRIVER_NAME "MyDevice"

// このデバイスドライバで取り扱うデバイスのマッチングテーブル
// dts内の下記に対応する
// i2c@7e804000 { /* i2c-1 */
//   ...
//   mydevice@6b { /* 0x6b : ジャイロセンサのアドレス */
//     compatible = "mycompany,myoriginaldevice";
//     reg = <0x6b>;
//   };
//   ...

static const struct of_device_id mydevice_of_match_table[] = {
  {.compatible = "mycompany,myoriginaldevice",},
  {},
};
MODULE_DEVICE_TABLE(of, mydevice_of_match_table);

// このデバイスドライバで取り扱うデバイスを識別するテーブルを登録する.
static struct i2c_device_id mydevice_i2c_idtable[] = {
  {"MyI2CDevice", 0},
  {}
};
MODULE_DEVICE_TABLE(i2c, mydevice_i2c_idtable);

static int mydevice_i2c_probe(struct i2c_client *client, const struct i2c_device_id *id)
{
  printk("mydevice_i2c_probe\n");
  if (id != NULL) printk("id.name = %s, id.driver_data = %ld\n", id->name, id->driver_data);
  if (client != NULL) printk("slave address = 0x%02X\n", client->addr);

  // 通常はここで, このデバドラでサポートしているデバイスかどうかチェックする.
  
  int version;
  version = i2c_smbus_read_byte_data(client, 0x0f);
  printk("id = 0x%02X\n", version);

  return 0;
}

static int mydevice_i2c_remove(struct i2c_client *client)
{
  printk("mydevice_i2c_remove\n");
  return 0;
}

static struct i2c_driver mydevice_driver = {
  .driver = {
    .name = DRIVER_NAME,
    .owner = THIS_MODULE,
    .of_match_table = mydevice_of_match_table,
  },
  .id_table = mydevice_i2c_idtable,
  .probe = mydevice_i2c_probe,
  .remove = mydevice_i2c_remove,
};

module_i2c_driver(mydevice_driver);
