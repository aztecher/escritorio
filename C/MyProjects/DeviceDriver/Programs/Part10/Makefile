CFILES := myDeviceDriver.c

obj-m := MyDeviceModule.o
MyDeviceModule-objs := $(CFILES:.c=.o)

ccflags-y += -std=gnu99 -Wall -Wno-declaration-after-statement

all:
	make -C /lib/modules/$(shell uname -r)/build M=$(shell pwd) modules

get_memory:
	gcc -o get_memory get_memory.c -L/opt/vc/lib -lbcm_host

clean:
	make -C /lib/modules/$(shell uname -r)/build M=$(shell pwd) clean
