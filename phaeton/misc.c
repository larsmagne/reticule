#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <gmime/gmime.h>
#include <dirent.h>
#include <errno.h>
#include <time.h>
#include <ctype.h>

#include "config.h"
#include "phaeton.h"
#include "misc.h"
#include <util.h>

char *reticule_home = RETICULE_HOME;
char *spool_directory = SPOOL_DIRECTORY;

GHashTable *group_table = NULL;
char *reverse_group_table[MAX_GROUPS];

int max_group_id = 0;

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

char *reticule_file_name(char *name) {
  static char file_name[1024];
  strcpy(file_name, reticule_home);
  strcat(file_name, "/");
  strcat(file_name, name);
  return file_name;
}

void read_groups_file(void) {
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

char *get_group_directory(const char *group) {
  static char file_name[1024];
  char *f = file_name + strlen(spool_directory);
  
  snprintf(file_name, 1024, "%s/%s", spool_directory, group);
  
  while (*f != 0) {
    if (*f == '.')
      *f = '/';
    f++;
  }

  return file_name;
}

char *mrealloc(char *buffer, int old_size, int new_size) {
  char *new = cmalloc(new_size);
  memcpy(new, buffer, old_size);
  free(buffer);
  return new;
}
