#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <fcntl.h>
#include <getopt.h>
#include <gmime/gmime.h>
#include <dirent.h>
#include <errno.h>
#include <time.h>

#include "config.h"
#include "phaeton.h"
#include "misc.h"
#include <util.h>

#define MAX_FILE_NAME 1024
#define MAX_XREFS 10
#define MAX_HEADER_LENGTH 1024
#define INITIAL_BUFFER_ENTRIES 1024
#define O_STREAMING    04000000

static char *buffer = NULL;
static int max_article = 0;
static int buffer_entries = 0;

struct option long_options[] = {
  {"spool", 1, 0, 's'},
  {"reticule", 1, 0, 'r'},
  {"help", 0, 0, 'h'},
  {0, 0, 0, 0}
};

typedef struct {
  int group_id;
  int article;
} post;

typedef struct {
  int article;
  char message_id[MAX_HEADER_LENGTH];
  char references[MAX_HEADER_LENGTH];
  char subject[MAX_HEADER_LENGTH];
  char from[MAX_HEADER_LENGTH];
  char xref[MAX_HEADER_LENGTH];
  char original_message_id[MAX_HEADER_LENGTH];
  time_t date;
  int chars;
  int lines;
  int spamp;

  int ncrossposts;
  post *crossposts;
} parsed_article;

int parse_args(int argc, char **argv) {
  int option_index = 0, c;
  while (1) {
    c = getopt_long(argc, argv, "hs:r:", long_options, &option_index);
    if (c == -1)
      break;

    switch (c) {
    case 's':
      news_spool = optarg;
      break;
      
    case 'r':
      reticule_home = optarg;
      break;
      
    case 'h':
      printf ("Usage: novgen [--spool <directory>] group\n");
      break;

    default:
      break;
    }
  }

  return optind;
}

void extend_buffer(int article) {
  int new_size;
  
  if (article < buffer_entries)
    return;
  
  if (buffer_entries == 0)
    new_size = INITIAL_BUFFER_ENTRIES;
  else
    new_size = buffer_entries * 2;

  buffer = mrealloc(buffer, buffer_entries * NOV_BLOCK_SIZE, 
		    new_size * NOV_BLOCK_SIZE);

  buffer_entries = new_size;
  max_article = article;
}

void mstrlcpy(char *to, const char *from, int size) {
  strncpy(to, from, size);
  *(to + size) = 0;
}

void clean_header(char *to) {
  while (*to) {
    if (*to == '\t' || 
	*to == '\n')
      *to = ' ';
    to++;
  }
}

void hstrcpy(char *to, const char *from) {
  if (from == NULL)
    *to = 0;
  else {
    mstrlcpy(to, from, MAX_HEADER_LENGTH);
    clean_header(to);
  }
}

void count_part(GMimePart* part, gpointer npa) {
  parsed_article *pa = (parsed_article*) npa;
  int length;
  const char *content;

  content = g_mime_part_get_content(part, &length);
  pa->chars += length;
  
  if (content != NULL) {
    while ((content = strchr(content, '\n')) != NULL) {
      content++;
      pa->lines++;
    }
  }
}

parsed_article *parse_file(const char *file_name) {
  static parsed_article pa;
  GMimeStream *stream;
  GMimeMessage *msg = NULL;
  int offset;
  int file;

  printf("%s\n", file_name);

  if ((file = open(file_name, O_RDONLY|O_STREAMING)) == -1) {
    fprintf(stderr, "Can't open %s\n", file_name);
    return NULL;
  }

  stream = g_mime_stream_fs_new(file);
  msg = g_mime_parser_construct_message(stream);
  g_mime_stream_unref(stream);

  if (msg != 0) {
    hstrcpy(pa.from, g_mime_message_get_header(msg, "From"));
    hstrcpy(pa.subject, g_mime_message_get_subject(msg));
    hstrcpy(pa.message_id, g_mime_message_get_message_id(msg));
    hstrcpy(pa.references, g_mime_message_get_header(msg, "references"));
    hstrcpy(pa.xref, g_mime_message_get_header(msg, "xref"));
    hstrcpy(pa.original_message_id,
	    g_mime_message_get_header(msg, "original-message-id"));
    g_mime_message_get_date(msg, &pa.date, &offset);

    if (pa.xref != NULL &&
	strstr(pa.xref, "gmane.spam.detected") != NULL)
      pa.spamp = 1;
    else
      pa.spamp = 0;

    g_mime_message_foreach_part(msg, count_part, (gpointer) &pa);

    g_mime_object_unref(GMIME_OBJECT(msg));
  
  }
  close(file);
  return &pa;
}

void parse_xref(char *s, post *crosspost) {
  char *art = strchr(s, ':');
  
  *art = 0;
  crosspost->article = atoi(art + 1);
  crosspost->group_id = group_name_to_id(s);
}

int string_header_length(parsed_article *pa) {
  return strlen(pa->message_id) + 1 +
    strlen(pa->references) + 1 +
    strlen(pa->from) + 1 +
    strlen(pa->subject) + 1;
}

int min(int a, int b) {
  if (a < b)
    return a;
  else
    return b;
}

void shorten_string_headers(parsed_article *pa) {
  char *s;
  if ((s = strrchr(pa->references, ' ')) != NULL) {
    *s = 0;
    return;
  }

  if (strlen(pa->subject) > 10)
    *(pa->subject + min(10, strlen(pa->subject) - 10)) = 0;

  if (strlen(pa->from) > 10)
    *(pa->subject + min(10, strlen(pa->from) - 10)) = 0;

  printf("shortened to %s, %s, %s\n", pa->subject, pa->from, pa->references);
}

void parse_xrefs(parsed_article *pa) {
  char xref[MAX_HEADER_LENGTH];
  char *s;
  int ncrossposts = 0;
  static post crossposts[MAX_XREFS];

  if (pa->xref == NULL) {
    pa->ncrossposts = 0;
    return;
  }
    
  strncpy(xref, pa->xref, MAX_HEADER_LENGTH - 1);
  
  s = strtok(xref, " ");
      
  while (s && ncrossposts < MAX_XREFS) {
    if (strchr(s, ':'))
      parse_xref(s, &crossposts[ncrossposts++]);
    s = strtok(NULL, " ");
  }
}


void generate_article(parsed_article *pa) {
  char *buf = buffer + pa->article * NOV_BLOCK_SIZE;
  unsigned char status = 128;
  int i;

  if (pa->spamp)
    status |= 32;

  status |= pa->ncrossposts;

  *buf++ = status;

  for (i = 0; i < pa->ncrossposts; i++) {
    *(short*)buf++ = (pa->crossposts[i]).group_id;
    *(int*)buf++ = (pa->crossposts[i]).article;
  }

  *(int*)buf++ = (int)pa->date;
  *(int*)buf++ = (int)pa->lines;
  *(int*)buf++ = (int)pa->chars;
  
  while (string_header_length(pa) > NOV_BLOCK_SIZE - 3 * 4 - 1 - 1) 
    shorten_string_headers(pa);

  strcat(buf, pa->subject);
  strcat(buf, pa->from);
  strcat(buf, pa->message_id);
  strcat(buf, pa->references);
}

void generate_file(const char *file_name, int article) {
  parsed_article *pa = parse_file(file_name);

  extend_buffer(article);
  parse_xrefs(pa);
  if (pa != NULL) {
    pa->article = article;
    generate_article(pa);
  }
}


void generate_group(const char *group) {
  char *dir_name = get_group_directory(group);
  DIR *dirp;
  struct dirent *dp;
  char file_name[MAX_FILE_NAME];
  struct stat stat_buf;

  if ((dirp = opendir(dir_name)) == NULL)
    return;
    
  while ((dp = readdir(dirp)) != NULL) {
    snprintf(file_name, MAX_FILE_NAME, "%s/%s", dir_name,
	     dp->d_name);

    if (stat(file_name, &stat_buf) == -1) {
      fprintf(stderr, "Couldn't stat file %s", file_name);
      break;
    }
    
    if (! S_ISDIR(stat_buf.st_mode) &&
	is_number(dp->d_name)) {
      generate_file(file_name, atoi(dp->d_name));
    }
  }
  closedir(dirp);
}

int main(int argc, char **argv) 
{
  int dirn;
  char *group;

  g_mime_init(0);
  read_groups_file();

  dirn = parse_args(argc, argv);
  
  if (dirn < argc) {
    group = argv[dirn];
    printf("Generating NOV file for %s\n", group);
    generate_group(group);
  } else {
    generate_group("gmane.discuss");
    //fprintf(stderr, "No group name given\n");
  }

  exit(0);
}
