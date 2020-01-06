#include<stdio.h>

int main() {
  int high = 0b10000;
  int low = 0b0000;
  int shift = ( 1 << 4 );

  printf ("case of High: %#x\n", high);
  printf ("case of Low : %#x\n", low);
  printf ("4 bit shift : %#x\n", shift);

  int hval = (high & ( 1 << 4 ));
  int lval = (low  & ( 1 << 4 ));

  printf ("case of High: %#x\n", hval);
  printf ("case of Low : %#x\n", lval);

  int ish = hval != 0;
  int isl = lval != 0;

  printf ("High compare to 0: %#x\n", ish);
  printf ("Low  compare to 0: %#x\n", isl);

  printf ("test: %d\n", (10 != 0));

  printf ("%c\n", (1 + '0'));
  printf ("%c\n", (0 + '0'));
  printf ("%c\n", (10 + '0'));
}

