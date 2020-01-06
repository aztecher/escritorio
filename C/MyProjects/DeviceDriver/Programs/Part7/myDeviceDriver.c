/*
 * procfs用インターフェース(ドライバ)作成
 *
 * Date :  2018.08.24
 * @author Mikiya Michishita
 */

#include <linux/module.h>
#include <linux/fs.h>
#include <linux/proc_fs.h>
#include <linux/uaccess.h>

// Device Information
MODULE_LICENSE("Dual BSD/GPL");
#define DRIVER_NAME "MyDevice" // /proc/device 等で表示されるデバイス名
#define PROC_NAME "MyDevice_name" // /procに作るprocfsの名前

// procfsテスト用変数
static char proc_test_string[16];
static int flag_read = 0;

// /proc/MyDevice_test にアクセスしたときに呼ばれる関数
static int mydevice_proc_open(struct inode *inode, struct file *file)
{
  printk ("mydevice_proc_open\n");
  flag_read = 0;
  return 0;
}

// /proc/MyDevice_test のread時に呼ばれる関数
static ssize_t mydevice_proc_read(struct file *filp, char __user *buf, size_t count, loff_t *f_pos)
{
  printk ("mydevice_proc_read\n");

  if (flag_read == 0){
    int len;
    len = sprintf(buf, "%s\n", proc_test_string);
    flag_read = 1;
    return len;
  } else {
    return 0;
  }
}

// /proc/MyDevice_test のwrite時に呼ばれる関数
static ssize_t mydevice_proc_write(struct file *filp, const char __user *buf, size_t count, loff_t *f_pos)
{
  printk ("mydevice_proc_write\n");

  // カーネル空間からユーザ空間へコピーする
  // (readで読んだ値をコピーする)
  // 文字列のサイズが違うので, サイズを確保してコピー
  if (count > sizeof(proc_test_string)) count = sizeof(proc_test_string) - 1;
  if (copy_from_user(proc_test_string, buf, count)) {
    return -EFAULT;
  }
  // 終端文字設定?
  proc_test_string[count] = '\0';
  return count;
}

// procfs用のハンドラテーブル
static struct file_operations mydevice_proc_fops = {
  .owner = THIS_MODULE,
  .open = mydevice_proc_open,
  .read = mydevice_proc_read,
  .write = mydevice_proc_write,
};

// ロード(insmod)時に呼ばれる関数
static int mydevice_init(void)
{
  printk ("mydevice_init\n");

  struct proc_dir_entry *entry;

  // procfsを作成する
  entry = proc_create(PROC_NAME, S_IRUGO | S_IWUGO, NULL, &mydevice_proc_fops);
  if (entry == NULL) {
    printk(KERN_ERR "proc_create\n");
    return -ENOMEM;
  }

  return 0;
}

// アンロード(rmmod)時に呼ばれる関数
static void mydevice_exit(void)
{
  printk ("mydevice_exit\n");

  // procfsを取り除く.
  remove_proc_entry(PROC_NAME, NULL);
}

module_init(mydevice_init);
module_exit(mydevice_exit);
