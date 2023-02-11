#include <stdio.h>
int main(void){
  const char *const green = "\033[0;40;31m";
  const char *const normal = "\033[0m";
  printf("%sHello World%s\n", green, normal);
  return 0;
}