#include <stdio.h>
#include <stdint.h>

extern int entry();

int main(void) {
  int64_t result = entry();
  printf("%ld\n", result);
}
