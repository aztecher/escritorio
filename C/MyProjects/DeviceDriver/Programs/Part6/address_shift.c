#include <stdio.h>

#define REG_ADDR_BASE 0x3F000000
#define REG_ADDR_GPIO_BASE (REG_ADDR_BASE + 0x00200000)
#define REG_ADDR_GPIO_LENGTH 4096
#define REG_ADDR_GPIO_GPFSEL_0     0x0000
#define REG_ADDR_GPIO_OUTPUT_SET_0 0x001C
#define REG_ADDR_GPIO_OUTPUT_CLR_0 0x0028
#define REG_ADDR_GPIO_LEVEL_0      0x0034

#define REG(addr) (*((volatile unsigned int*) (addr)))
#define DUMP_REG(addr) printf("%08X\n", REG(addr))

void print_hex(unsigned int *hex) {
  printf ("%#08x\n", hex);
  printf ("%#010x\n", hex);
  printf ("0x%08x\n", hex);

  printf ("%08x\n", hex);
  printf ("%08X\n", hex);
}

int main()
{
  /* DUMP_REG(REG_ADDR_BASE); */
}
