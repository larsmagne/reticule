#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
#include <string.h>

#include "config.h"
#include "dispatch.h"
#include <util.h>

int dispatch(FILE *client, char **command, dispatcher *disp) {
  while (disp->command != NULL) {
    if (! strcasecmp(command[0], disp->command)) {
      return (*disp->handler)(client, command + 1);
    } else {
      disp++;
    }
  }

  return -1;
}
