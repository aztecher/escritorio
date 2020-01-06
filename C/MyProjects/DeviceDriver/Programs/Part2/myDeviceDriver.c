#include<linux/module.h>
#include<linux/fs.h>

#define DRIVER_NAME "MyDevice_NAME"
#define DRIVER_MAJOR 63

// open handler
static int mydevice_open(struct inode *inode, struct file *file)
{
  printk("mydevice_open");
  return 0;
}

// close handler
static int mydevice_close(struct inode *inode, struct file *file)
{
  printk("mydevice_close");
  return 0;
}

// read handler
static ssize_t mydevice_read(struct file *filp, char __user *buf, size_t count, loff_t *f_pos)
{
  printk("mydevice_read");
  buf[0] = 'A';
  return 1;
}

// write handler
static ssize_t mydevice_write(struct file *filp, const char __user *buf, size_t count, loff_t *f_pos)
{
  printk("mydevice_write");
  return 1;
}

// handler table corresponding to various system calls
struct file_operations s_mydevice_fops = {
  .open = mydevice_open,
  .release = mydevice_close,
  .read = mydevice_read,
  .write = mydevice_write,
};

// insmod handler
static int mydevice_init(void)
{
  printk("mydevice_init\n");

  // register my driver to kernel
  register_chrdev(DRIVER_MAJOR, DRIVER_NAME, &s_mydevice_fops);
  return 0;
}

// rmmod handler
static void mydevice_exit(void)
{
  printk("mydevice_exit\n");

  // unregister my driver
  unregister_chrdev(DRIVER_MAJOR, DRIVER_NAME);
}

module_init(mydevice_init);
module_exit(mydevice_exit);
