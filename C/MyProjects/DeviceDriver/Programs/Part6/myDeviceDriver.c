#include <linux/module.h>
#include <linux/fs.h>
#include <linux/cdev.h>
#include <linux/device.h>
#include <linux/uaccess.h>
#include <linux/slab.h>
#include <asm/io.h>

// ioctl handler include
#include "myDeviceDriver.h"

// raspi peripherals macro
#define REG_ADDR_BASE 0x3F000000
#define REG_ADDR_GPIO_BASE (REG_ADDR_BASE + 0x00200000)
#define REG_ADDR_GPIO_GPFSEL_0 0x0000
#define REG_ADDR_GPIO_OUTPUT_SET_0 0x001C
#define REG_ADDR_GPIO_OUTPUT_CLR_0 0x0028
#define REG_ADDR_GPIO_LEVEL_0 0x0034
#define REG(addr) (*((volatile unsigned int*) (addr)))
#define DUMP_REG(addr) printk("%08X\n", REG(addr))

// device information
MODULE_LICENSE("Dual BSD/GPL");
#define DRIVER_NAME "MyDevice"
static const unsigned int MINOR_BASE = 0;
static const unsigned int MINOR_NUM = 1;
static unsigned int mydevice_major; // major number of device driver
static struct cdev mydevice_cdev; // charactor device object
static struct class *mydevice_class = NULL; // device driver class object

/*** ##### device ioctl handler ##### ***/
// variables for using test of ioctl
// you should use 'private_data' member of 'file' structure in your program.
static struct mydevice_values stored_values;

// ioctl handler (you have to add this function to handler table)
static long mydevice_ioctl(struct file *filp, unsigned int cmd, unsigned long arg)
{
  printk("mydevice_ioctl\n");

  // command switch and implementation
  // in arg variable, the pointer of parameters are stored.
  // so you read it with casting (same as read/write).
  switch(cmd) {
  case MYDEVICE_SET_VALUES:
    printk("MYDEVICE_SET_VALUES\n");
    if (copy_from_user(&stored_values, (void __user *)arg, sizeof(stored_values))) {
      return -EFAULT;
    }
    break;
  case MYDEVICE_GET_VALUES:
    printk("MYDEVICE_GET_VALUES\n");
    if (copy_to_user((void __user *)arg, &stored_values, sizeof(stored_values))) {
      return -EFAULT;
    }
    break;
  default:
    printk(KERN_WARNING "unsupported command %d\n", cmd);
    return -EFAULT;
  }
  return 0;
}

// ##### device open/read/write/close handler ##### //
// implement the Raspi Peripheral address I/O access in these handlers

// open handler
static int mydevice_open(struct inode *inode, struct file *file)
{
  printk("mydevice_open");

  // ARM Physical Address -> Kernel Virtual address Mapping
  // Peripheral (GPFSEL0) address mapping
  int address = (int)ioremap_nocache(REG_ADDR_GPIO_BASE + REG_ADDR_GPIO_GPFSEL_0, 4);

  // set GPIO4 is OUTPUT
  REG(address) = 1 << 12;

  // Unmap GPSEL0 map
  iounmap((void*) address);

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

  // ARM Physical Address -> Kernel Virtual address Mapping
  // Peripheral (GPFLEV0) address mapping
  // (GPLEV0 is 32bit data -> get 4byte (32bit) from GPLEV0 start address)
  int address = (int)ioremap_nocache(REG_ADDR_BASE + REG_ADDR_GPIO_LEVEL_0, 4);

  // Get state of GPIO4 from GPLEV0 (0(Low)/1(High))
  //
  // (consider under 4 bit)
  // GPIO4 is High -> (Register is) ...1000 : (...1000) & (...1000) = ...1000
  // GPIO4 is Low  -> (Register is) ...0000 : (...0000) & (...1000) = ...0000
  //
  // if the (REG(address) & (1 << 4)) is    0 -> val = 0
  // if the (REG(address) & (1 << 4)) isn't 0 -> val = 1
  //
  // (ex)
  // printf ("%d\n", (10 != 0)); // > 1
  //
  int val = (REG(address) & ( 1 << 4 )) != 0;

  // return the state of GPIO to user as charactor
  // put the data to user space buffer.
  // this buffer is read by user.
  //
  // *** WARNING ***
  // if you put your data of int value, you would use 'itoa' function.
  // but, in kernel space, you can't use this function.
  // Now you have to print the '0/1' of int value, you remember
  // (char) '0' -> (int) 48
  // (char) '1' -> (int) 49
  // so, you add your int 0/1 data to '0', so
  // 0 + '0' = 48
  // 1 + '0' = 49
  // so, you can print it to '0' or '1'.
  put_user(val + '0', &buf[0]);

  // Unmap GPLEV0 map
  iounmap((void*)address);

  return count;
}

// write handler
static ssize_t mydevice_write(struct file *filp, const char __user *buf,  size_t count, loff_t *f_pos)
{
  printk("mydevice_write");

  int address;
  char outValue;

  // Get the GPIO4 output value from user space buffer
  // this buffer is setted by user input.
  get_user(outValue, &buf[0]);

  // ARM Physical Address -> Kernel Virtual address Mapping
  // Peripheral (GPFLEV0) address mapping
  // User set output '1' -> set GPSET0 of GPIO4
  // User set output '0' -> set GPCLR0 of GPIO4
  // (GPSET0/GPCLR0 is 32bit data -> get 4 byte (32bit) from GPSET0/GPCLR0 start address)
  if (outValue == '1') {
    address = (int)ioremap_nocache(REG_ADDR_GPIO_BASE + REG_ADDR_GPIO_OUTPUT_SET_0, 4);
  } else {
    address = (int)ioremap_nocache(REG_ADDR_GPIO_BASE + REG_ADDR_GPIO_OUTPUT_CLR_0, 4);
  }

  // set GPSET0/GPCLR0
  REG(address) = 1 << 4;
  
  // Unmap GPSET0/GPCLR0 map
  iounmap((void*)address);

  return count;
}

// systemcall handler table
struct file_operations s_mydevice_fops = {
  .open = mydevice_open,
  .release = mydevice_close,
  .read = mydevice_read,
  .write = mydevice_write,
  .unlocked_ioctl = mydevice_ioctl,
  .compat_ioctl = mydevice_ioctl, // for 32-bit App
};

// ##### Kernel Module load / unload handlers ##### //
// insmod handler
static int mydevice_init(void)
{
  printk ("mydevice_init\n");

  int alloc_ret = 0;
  int cdev_err = 0;
  dev_t dev;

  // *** Creating Device and Register Kernel ***
  // 1. get major number (dinamically)
  alloc_ret = alloc_chrdev_region(&dev, MINOR_BASE, MINOR_NUM, DRIVER_NAME);
  if (alloc_ret != 0) {
    printk(KERN_ERR "alloc_chrdev_region = %d\n", alloc_ret);
    return -1;
  }

  // 2. fetch major number from dev object.
  mydevice_major = MAJOR(dev);
  dev = MKDEV(mydevice_major, MINOR_BASE);

  // 3. initialize cdev structure and register systemcall handler to it
  cdev_init(&mydevice_cdev, &s_mydevice_fops);
  mydevice_cdev.owner = THIS_MODULE;

  // 4. register cdev to kernel using 'cdev' and 'dev'
  cdev_err = cdev_add(&mydevice_cdev, dev, MINOR_NUM);
  if (cdev_err != 0) {
    printk(KERN_ERR "cdev_add = %d\n", alloc_ret);
    unregister_chrdev_region(dev, MINOR_NUM);
    return -1;
  }

  // *** Automatically Creating /dev/mydevice* ***
  // 1. register class of this device (create /sys/class/mydevice/)
  mydevice_class = class_create(THIS_MODULE, "mydevice");
  if (IS_ERR(mydevice_class)) {
    printk(KERN_ERR "class_create\n");
    cdev_del(&mydevice_cdev);
    unregister_chrdev_region(dev, MINOR_NUM);
    return -1;
  }

  // 2. create /sys/class/mydevice/mydevice* to create /dev/mydevice*
  for (int minor = MINOR_BASE; minor < MINOR_BASE + MINOR_NUM; minor++) {
    device_create(mydevice_class, NULL, MKDEV(mydevice_major, minor), NULL, "mydevice%d", minor);
  }

  return 0;
}

// rmmod handler
static void mydevice_exit(void)
{
  printk ("mydevice_exit\n");

  dev_t dev = MKDEV(mydevice_major, MINOR_BASE);

  // 1. delete /sys/class/mydevice/mydevice* 
  for (int minor = MINOR_BASE; minor < MINOR_BASE + MINOR_NUM; minor++) {
    device_destroy(mydevice_class, MKDEV(mydevice_major, minor));
  }

  // 2. delete /sys/class/mydevice/
  class_destroy(mydevice_class);

  // 3. delete device driver from kernel
  cdev_del(&mydevice_cdev);

  // 4. delete major number
  unregister_chrdev_region(dev, MINOR_NUM);
}

// register handler
module_init(mydevice_init);
module_exit(mydevice_exit);

