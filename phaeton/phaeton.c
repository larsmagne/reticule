#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#if defined(__FreeBSD__)
#  include <sys/mman.h>
#endif
#include <fcntl.h>
#include <getopt.h>
#include <gmime/gmime.h>
#include <dirent.h>
#include <errno.h>
#include <time.h>
#include <ctype.h>

#include "config.h"
#include "phaeton.h"
#include "misc.h"
#include <util.h>

int server_port = SERVER_PORT;
char *version = VERSION;
char *host_name = HOST_NAME;

int active_file = 0;
char *selected_group = NULL;
unsigned char *group_map = NULL;
int group_map_length = 0;

void message(FILE *client, int code, char *message) {
  if (code == 0)
    fprintf(client, "%s\r\n", message);
  else
    fprintf(client, "%d %s\r\n", code, message);
}

void open_active_file() {
  if ((active_file = open(reticule_file_name("active"), O_RDONLY)) == 0) {
    fprintf(stderr, "Couldn't find active file\n");
    return;
  }
}

int select_group(char *group) {
  int fd;
  char *map_file_name = get_overview_name(group, ".MAP");
  loff_t map_size;

  fprintf(stderr, "Using %s\n", map_file_name);

  if (group_map != NULL) {
    free(group_map);
    group_map = NULL;
    group_map_length = 0;
  }

  if (selected_group != NULL) {
    free(selected_group);
    selected_group = NULL;
  }
  
  if ((fd = open(map_file_name, O_RDONLY)) < 0) {
    fprintf(stderr, "Couldn't open %s\n", map_file_name);
    return 0;
  }

  map_size = file_size(fd);
  group_map_length = map_size;
  group_map = (unsigned char*)malloc(map_size);
  read_block(fd, group_map, map_size);
  close(fd);

  selected_group = strdup(group);
  client.selected_group = 0;

  return 1;
}

int message_present(int article) {
  int offset = article / 8;

  if (offset > group_map_length)
    return 0;

  return (*(group_map + offset)) & (1 << (article % 8));
}

int selectedp() {
  int selectedp = (selected_group != NULL);
  if (selectedp && client.selected_group == 0) {
    client.selected_group = 1;
    client.groups++;
  }
  return selectedp;
}

void tputs(char *string, FILE *fp) {
  client.bytes += strlen(string);
  fputs(string, fp);
}
