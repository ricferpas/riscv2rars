
#include "mips-runtime.h"

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include "keyio.h"

static bool keyio = false;
static void disable_keyio(void) {
  if (keyio) {
    keyio_reset();
    keyio = false;
  }
}
static void enable_keyio(void) {
  if (!keyio) {
    keyio_init();
    keyio = true;
  }
}

void print_integer(int a0) {
  printf("%d", a0);
}

void read_string(char *a0, int a1) {
  fflush(stdout);
  disable_keyio();
  if (fgets(a0, a1, stdin) == NULL) {
    printf("\nError leyendo de la entrada estándar.\n");
    exit(-1);
  }
  if (getenv("MIPS_ECHO")) {
    fputs(a0, stdout);
  }
}

void print_character(char a0) {
  putchar(a0);
}

void print_string(const char *a0) {
  fputs(a0, stdout);
}

unsigned long long get_time(void) {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  unsigned long long ret = ((unsigned long long) tv.tv_sec) * 1000 + tv.tv_usec / 1000;
  return ret;
}

int read_character(void) {
  enable_keyio();
  return keyio_read_key();
}

void clear_screen(void) {
  print_string("\x1b\x5b\x32\x4a");
  print_string("\x1b\x5b\x30\x3b\x30\x66");  
}

void mips_exit(int exitcode) {
  exit(exitcode);
}

/* ignora el id */
int random_int(int id) {
  return rand();
}

/* ignora el id */
int random_int_range(int id, int max) {
  return random_int(id) % (max + 1); // TODO: +1 ? ver Mars
}
