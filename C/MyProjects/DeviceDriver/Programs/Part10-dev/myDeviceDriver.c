/*
 * I2C Device Driver
 *
 * これまでのコードの集大成
 * 独自構造体などを利用しているが, やっていることはこれまでと同じ.
 *
 * Date :  2018.08.24
 * @author Mikiya Michishita
 */

#include <linux/module.h>
#include <linux/i2c.h>

#include <linux/cdev.h> // cdev
#include <linux/device.h> // struct class (device class)

// Device Information
MODULE_LICENSE("Dual BSD/GPL");
#define DRIVER_NAME "MyDevice"

// dev/mydevice* で作成するデバイスファイルの数に関わる値
static const unsigned int MINOR_BASE = 0;
static const unsigned int MINOR_NUM  = 1;

// I2Cデバイスの管理情報
// このデバドラで取り扱うI2Cデバイス
enum mydevice_i2c_model {
  MYDEVICE_MODEL_A = 0,
  MYDEVICE_MODEL_NUM,
};

// このデバドラで取り扱うデバイスを識別するテーブルを登録する.
static struct i2c_device_id mydevice_i2c_idtable[] = {
  {"MyI2CDevice", MYDEVICE_MODEL_A},
  {}
};
MODULE_DEVICE_TABLE(i2c, mydevice_i2c_idtable);

// 各i2cデバイス(client)に紐づく情報.
// probe時に設定して, i2c_set_clientdataで保持しておく.
//
// cdev : probeされたI2Cデバイス(client)とcdevを対応付けるために必要
//        open時にcontainer_ofで探す
// mydevice_major : このデバイスドライバのメジャー番号(動的に決める)
// mydevice_class : デバドラのクラスオブジェクト
struct mydevice_device_info {
  struct cdev cdev;
  unsigned int mydevice_major;
  struct class *mydevice_class;
  struct i2c_client *client;
  // 他に必要なら追加する, mutexとか
};

// /dev/mydevice* のopenハンドラ
static int mydevice_open(struct inode *inode, struct file *file)
{
  printk("mydevice_open\n");

  // このopenを持つcdev(inode->i_cdev)を持つmydevice_device_infoを探す
  struct mydevice_device_info *dev_info;
  dev_info = container_of(inode->i_cdev, struct mydevice_device_info, cdev);
  if (dev_info == NULL || dev_info->client == NULL) {
    printk (KERN_ERR "container_of\n");
    return -EFAULT;
  }
  file->private_data = dev_info;
  printk("i2c address = %02X\n", dev_info->client->addr);

  return 0;
}

// /dev/mydevice* のcloseハンドラ
static int mydevice_close(struct inode *inode, struct file *file)
{
  printk("mydevice_close");
  return 0;
}

// /dev/mydevice* のreadハンドラ
static ssize_t mydevice_read(struct file *filp, char __user *buf, size_t count, loff_t *f_pos)
{
  printk("mydevice_read\n");

  struct mydevice_device_info *dev_info = filp->private_data;
  struct i2c_client *client = dev_info->client;

  int version;
  version = i2c_smbus_read_byte_data(client, 0x0f);
  return sprintf(buf, "id=0x%02X\n", version);
}

// /dev/mydevice* のwriteハンドラ
static ssize_t mydevice_write(struct file *filp, const char __user *buf, size_t count, loff_t *f_pos)
{
  printk("mydevice_write\n");
  return count;
}

// 各種システムコールに対応するハンドラテーブル
struct file_operations s_mydevice_fops = {
  .open = mydevice_open,
  .release = mydevice_close,
  .read = mydevice_read,
  .write = mydevice_write,
};

// /dev/mydevice* を作成する関数.
// I2Cデバイスが認識されたときに呼ばれるハンドラ内で呼ばれる.
static int mydevice_i2c_create_cdev(struct mydevice_device_info *dev_info)
{
  int alloc_ret = 0;
  int cdev_err = 0;
  dev_t dev;

  // メジャー番号を確保する
  alloc_ret = alloc_chrdev_region(&dev, MINOR_BASE, MINOR_NUM, DRIVER_NAME);
  if (alloc_ret != 0) {
    printk(KERN_ERR "alloc_chrdev_region = %d\n", alloc_ret);
    return -1;
  }

  // 取得したdev(=メジャー番号 + マイナー番号)
  // メジャー番号を取得して保存
  dev_info->mydevice_major = MAJOR(dev);
  dev = MKDEV(dev_info->mydevice_major, MINOR_BASE); // 不要?

  // cdev構造体の初期化とシステムコールハンドラテーブルの登録
  cdev_init(&dev_info->cdev, &s_mydevice_fops);
  dev_info->cdev.owner = THIS_MODULE;

  // このデバイスドライバ(cdev)をカーネルに登録する.
  cdev_err = cdev_add(&dev_info->cdev, dev, MINOR_NUM);
  if (cdev_err != 0) {
    printk(KERN_ERR "cdev_add = %d\n", alloc_ret);
    unregister_chrdev_region(dev, MINOR_NUM);
    return -1;
  }

  // このデバイスのクラス登録をする(/sys/class/mydevice/ を作る)
  dev_info->mydevice_class = class_create(THIS_MODULE, "mydevice");
  if (IS_ERR(dev_info->mydevice_class)) {
    printk(KERN_ERR "class_create\n");
    cdev_del(&dev_info->cdev);
    unregister_chrdev_region(dev, MINOR_NUM);
    return -1;
  }

  // /sys/class/mydevice/mydevice* を作る
  for (int minor = MINOR_BASE; minor < MINOR_BASE + MINOR_NUM; minor++) {
    device_create(dev_info->mydevice_class, NULL, MKDEV(dev_info->mydevice_major, minor), NULL, "mydevice%d", minor);
  }

  return 0;
}

// /dev/mydevice* を削除する関数.
// I2Cデバイスが取り除かれたときに呼ばれるハンドラ内で呼ばれる.
static void mydevice_i2c_delete_cdev(struct mydevice_device_info *dev_info)
{
  dev_t dev = MKDEV(dev_info->mydevice_major, MINOR_BASE);

  // /sys/class/mydevice/mydevice* を削除する
  for (int minor = MINOR_BASE; minor < MINOR_BASE + MINOR_NUM; minor++) {
    device_destroy(dev_info->mydevice_class, MKDEV(dev_info->mydevice_major, minor));
  }

  // このデバイスのクラス登録を取り除く(/sys/class/mydevice/を削除する)
  class_destroy(dev_info->mydevice_class);

  // このデバイスドライバ(cdev)をカーネルから取り除く
  cdev_del(&dev_info->cdev);

  // このデバイスドライバで使用していたメジャー番号の登録を取り除く
  unregister_chrdev_region(dev, MINOR_NUM);
}

// I2Cデバイスが認識されたときに呼ばれるハンドラ
static int mydevice_i2c_probe(struct i2c_client *client, const struct i2c_device_id *id)
{
  printk("mydevice_i2c_probe\n");
  printk("id.name = %s, id.driver_data = %d\n", id->name, (int)(id->driver_data));
  printk("slave address = 0x%02X\n", client->addr);

  // 通常はここで, このデバドラでサポートしているデバイスかどうかチェック

  // open/close/read/write でも i2c_clientは使うので, 保持する.
  struct mydevice_device_info *dev_info;
  dev_info = (struct mydevice_device_info*)devm_kzalloc(&client->dev, sizeof(struct mydevice_device_info), GFP_KERNEL);
  dev_info->client = client;
  i2c_set_clientdata(client, dev_info);

  // このデバイスドライバをキャラクタ型としてカーネルに登録する.
  // (/sys/class/mydevice/mydevice* を作る)
  if (mydevice_i2c_create_cdev(dev_info)) return -ENOMEM;

  return 0;
}

// I2Cデバドラが抜かれたときに呼ばれるハンドラ
static int mydevice_i2c_remove(struct i2c_client *client)
{
  printk("mydevice_i2c_remove\n");
  struct mydevice_device_info *dev_info;
  dev_info = i2c_get_clientdata(client);
  mydevice_i2c_delete_cdev(dev_info);

  return 0;
}

// I2C用デバイスドライバ
static struct i2c_driver mydevice_driver = {
  .driver = {
    .name = DRIVER_NAME,
    .owner = THIS_MODULE,
  },
  .id_table = mydevice_i2c_idtable,
  .probe = mydevice_i2c_probe,
  .remove = mydevice_i2c_remove,
};

// 本デバドラを, I2Cバスを使用するデバドラとして登録する.
// これまで作成していた mydevice_init/mydevice_exit を作っても
// やることがI2Cバスを利用するデバドラの登録なので,
// 簡易化する関数を利用する.
module_i2c_driver(mydevice_driver);


