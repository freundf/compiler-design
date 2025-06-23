#include <stdio.h>

int print(char c) {
  putchar(c);
  return 0;
}

int read() {
  return getchar();
}

int flush() {
  fflush(stdout);
  return 0;
}