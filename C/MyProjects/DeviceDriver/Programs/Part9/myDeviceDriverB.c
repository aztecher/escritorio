/*
 * 別モジュールの関数を呼び出すカーネルモジュール
 *
 * Date :  2018.08.24
 * @author Mikiya Michishita
 */

#include <linux/module.h>

// Device Information
MODULE_LICENSE("Dual BSD/GPL");
#define DRIVER_NAME "MyDeviceB"

// ロード時(insmod)時に呼ばれる関数
static int mydeviceb_init(void)
{
  printk("[B]: mydevice_init\n");

  // ヘッダーを使わない, お行儀の悪い書き方
  extern void mydevicea_func(void);
  mydevicea_func();

  return 0;
}

// アンロード時(rmmod)時に呼ばれる関数
static void mydeviceb_exit(void)
{
  printk("[B]: mydevice_exit\n");
}

module_init(mydeviceb_init);
module_exit(mydeviceb_exit);
