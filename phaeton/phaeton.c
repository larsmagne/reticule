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
#include <util.h>

char *reticule_home = RETICULE_HOME;
char *spool_directory = SPOOL_DIRECTORY;
int server_port = SERVER_PORT;
char *version = VERSION;
char *host_name = HOST_NAME;

static GHashTable *group_table = NULL;
static char *reverse_group_table[MAX_GROUPS];
int active_file = 0;
char *selected_group = NULL;
unsigned char *group_map = NULL;
int group_map_length = 0;

int max_group_id = 0;

void message(FILE *client, int code, char *message) {
  if (code == 0)
    fprintf(client, "%s\r\n", message);
  else
    fprintf(client, "%d %s\r\n", code, message);
}

char *reticule_file_name(char *name) {
  static char file_name[1024];
  strcpy(file_name, reticule_home);
  strcat(file_name, "/");
  strcat(file_name, name);
  return file_name;
}

void read_groups_file() {
  FILE *fp;
  char group[MAX_GROUP_NAME_LENGTH], *g;
  int group_id;
  
  group_table = g_hash_table_new(g_str_hash, g_str_equal);

  if ((fp = fopen(reticule_file_name("groups.dat"), "r")) == NULL) {
    fprintf(stderr, "Couldn't find groups.dat file\n");
    return;
  }

  while (fscanf(fp, "%s %d\n", group, &group_id) != EOF) {
    g = cmalloc(strlen(group)+1);
    strcpy(g, group);

    g_hash_table_insert(group_table, (gpointer)g, (gpointer)group_id);
    reverse_group_table[group_id] = g;
    if (group_id > max_group_id)
      max_group_id = group_id;
  }
  fclose(fp);
}

char *group_id_to_name(int id) {
  return reverse_group_table[id];
}

int group_name_to_id(char *group) {
  return (int)g_hash_table_lookup(group_table, (gpointer)group);
}

void open_active_file() {
  if ((active_file = open(reticule_file_name("active"), O_RDONLY)) == 0) {
    fprintf(stderr, "Couldn't find active file\n");
    return;
  }
}

char *get_overview_name(char *group, char *extension) {
  static char file_name[1024];
  char *f, *g = group;

  strcpy(file_name, reticule_home);
  strcat(file_name, "/overview/");
  f = file_name + strlen(file_name);
  while (*g) {
    *f++ = *g;
    *f++ = '/';
    if (strchr(g, '.')) 
      g = strchr(g, '.') + 1;
    else 
      break;
  } 
  *f = 0;

  strcat(file_name, group);
  strcat(file_name, extension);
  return file_name;
  
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

int isnumerical(char *string) {
  while (*string) {
    if (! isdigit(*string))
      return 0;
    string++;
  }
  return 1;
}

char *get_article_name(char *group, int article) {
  static char file_name[1024];
  char *f = file_name + strlen(spool_directory);
  
  snprintf(file_name, 1024, "%s/%s/%d", spool_directory, group, article);
  
  while (*f != 0) {
    if (*f == '.')
      *f = '/';
    f++;
  }

  return file_name;
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
