/*
 * 他のモジュールから呼べるような関数
 *
 * Date :  2018.08.24
 * @author Mikiya Michishita
 */
#include <linux/module.h>

// Device Information
MODULE_LICENSE("Dual BSD/GPL");
#define DRIVER_NAME "MyDeviceA"

// 他のカーネルモジュールから呼べるようにする関数
void mydevicea_func(void)
{
  printk("This is a message in mydevicea_func\n");
}
// カーネルのシンボルテーブルに登録する
EXPORT_SYMBOL(mydevicea_func);

// ロード(insmod)時に呼ばれる関数
static int mydevicea_init(void)
{
  printk("[A]: mydevice_init\n");
  mydevicea_func();

  return 0;
}

// アンロード(rmmod)時に呼ばれる関数
static void mydevicea_exit(void)
{
  printk("[A]: mydevice_exit\n");
}

module_init(mydevicea_init);
module_exit(mydevicea_exit);

