#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
#include <strings.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>

#include "config.h"
#include "phaeton.h"
#include "commands.h"
#include <util.h>

int com_hello(FILE *client, char **args) {
  fprintf(client, "200 %s Phaeton/%s server (Reticule) ready (posting ok).\r\n",
	  host_name, version);
  return 0;
}

int com_mode(FILE *client, char **args) {
  if (*args != NULL && ! strcasecmp(*args, "reader"))
    com_hello(client, NULL);
  else
    message(client, 500, "Unknown mode command");
  return 0;
}

int com_quit(FILE *client, char **args) {
  message(client, 205, "Goodbye");
  return 1;
}

void output_active_entry(FILE *client, int id) {
  int buffer[3];
  char *group;

  read_block(active_file, (char *)buffer, 12);
  if ((group = group_id_to_name(id)) != NULL) 
    fprintf(client, "%s %d %d y\r\n", group, buffer[1], buffer[0]);
}

int com_list_active(FILE *client, char **args) {
  int id, i;

  message(client, 215, "Newsgroups in form \"group high low flags\"");

  if (*args != NULL) {
    if ((id = group_name_to_id(*args)) != 0) {
      lseek(active_file, id * 4 * 3, SEEK_SET);
      output_active_entry(client, id);
    } 
  } else {
    lseek(active_file, 0, SEEK_SET);
    for (i = 0; i < max_group_id; i++) {
      output_active_entry(client, i);
    }
  }

  message(client, 0, ".");
  return 0;
}

int com_list(FILE *client, char **args) {
  if (*args == NULL || ! strcasecmp(*args, "active")) 
    com_list_active(client, (*args == NULL? args: args + 1));
  else
    message(client, 500, "Unknown list command");
  return 0;
}

int com_group(FILE *client, char **args) {
  int id;
  int buffer[3];

  if (*args == NULL)
    message(client, 501, "newsgroup");
  else if ((id = group_name_to_id(*args)) == 0)
    message(client, 411, "No such group");
  else {
    if (select_group(*args)) {
      lseek(active_file, id * 4 * 3, SEEK_SET);
      read_block(active_file, (char *)buffer, 12);
      fprintf(client, "211 %d %d %d %s\r\n", 
	      buffer[2], buffer[0], buffer[1], *args);
    } else {
      message(client, 401, "Error when selecting group");
    }
  }
  return 0;
}

void output_article(FILE *client, char *group, int article) {
  char *file = get_article_name(group, article);
  char buffer[1024];
  FILE *art;
  int new_line = 1;

  if ((art = fopen(file, "r")) == NULL) {
    message(client, 423, "Bad article number");
  } else  {
    fprintf(client, "220 %d article\r\n", article);
    while (fgets(buffer, 1024, art) != NULL) {
      if (new_line && *buffer == '.')
	fputs(".", client);
      fputs(buffer, client);

      if (strchr(buffer, '\n'))
	new_line = 1;
      else
	new_line = 0;
    }
    fputs(".\r\n", client);
  }
}

int com_article(FILE *client, char **args) {
  if (selected_group == NULL) {
    message(client, 412, "Not in a newsgroup");
  } else if (*args == NULL) {
    message(client, 413, "Name a message");
  } else if (isnumerical(*args)) {
    if (message_present(atoi(*args))) {
      output_article(client, selected_group, atoi(*args));
    } else {
      message(client, 423, "Bad article number");
    }
  } else {
    message(client, 413, "Not implemented");
  }

  return 0;
}

int com_help(FILE *client, char **args) {
  fputs("100 Valid commands\r\n", client);
  fputs("  mode reader\r\n", client);
  fputs("  list active\r\n", client);
  fputs("  group\r\n", client);
  fputs("  article\r\n", client);
  fputs("  help\r\n", client);
  fputs("  over\r\n", client);
  fputs(".\r\n", client);
  return 0;
}

void output_overview_line(FILE *client, char *buffer, int article) {
  int present = *buffer & 128;
  int suppressed = *buffer & 64;
  int spam = *buffer & 32;
  int crossposts = *buffer & 31;
  char *strings;
  const char *format = "%a, %_d %b %Y %H:%M:%S %z";
  struct tm *date;
  time_t time;
  char date_string[1024];

  if ((! present) || suppressed)
    return;

  fprintf(client, "%d\t", article);

  strings = buffer + 1 + (crossposts * 6) + (4 * 3);
  /* Subject */
  fputs(strings, client);
  fputs("\t", client);
  /* From */
  strings += strlen(strings) + 1;
  fputs(strings, client);
  fputs("\t", client);
  strings += strlen(strings) + 1;

  /* Date */
  time = *((time_t*)(buffer + 1 + (crossposts * 6)));
  date = localtime((const time_t *)&time);
  strftime(date_string, 1024, format, date);
  fputs(date_string, client);
  fputs("\t", client);

  /* Message-ID */
  fputs(strings, client);
  fputs("\t", client);
  strings += strlen(strings) + 1;

  /* References */
  fputs(strings, client);
  fputs("\t", client);
  strings += strlen(strings) + 1;

  /* Bytes */
  fprintf(client, "%d", *((int*)(buffer + 1 + (crossposts * 6) + 4 + 4)));
  fputs("\t", client);

  /* Lines */
  fprintf(client, "%d", *((int*)(buffer + 1 + (crossposts * 6) + 4)));
  fputs("\t", client);

  /* Xrefs */
  buffer++;
  fprintf(client, "Xref: %s", host_name);
  while (crossposts-- > 0) {
    fprintf(client, " %s:%d", group_id_to_name(*((short*)buffer)),
	    *((int*)(buffer+2)));
    buffer += 6;
  }
  fputs("\r\n", client);
}

void output_overview_lines(FILE *client, int fd, int start, int stop) {
  char buffer[4096];
  int ratio = 4096 / NOV_BLOCK_SIZE;
  int phase = 0;
  int i;

  fprintf(client, "224 %d-%d fields follow\r\n", start, stop);

  lseek(fd, start * NOV_BLOCK_SIZE, SEEK_SET);

  while (start <= stop) {
    if (phase == 0 || phase == 2) {
      read_block(fd, buffer, NOV_BLOCK_SIZE);
      output_overview_line(client, buffer, start++);
    } else {
      read_block(fd, buffer, NOV_BLOCK_SIZE * ratio);
      for (i = 0; i < ratio; i++)
	output_overview_line(client, buffer + i * NOV_BLOCK_SIZE, start++);
    }
    if (phase == 0 && (start % ratio) == 0 && start + ratio < stop)
      phase = 1;
    else if (phase == 1 && start + ratio > stop)
      phase = 2;
  }
  fputs(".\r\n", client);
}

void output_overview(FILE *client, char *spec) {
  char *file_name = get_overview_name(selected_group, ".NOV");
  int fd;
  char *dash;
  int start = 0, stop = 0;
  loff_t length;

  if ((fd = open(file_name, O_RDONLY)) == 0) {
    message(client, 433, "Couldn't find overview file");
  } else {
    if ((dash = strchr(spec, '-')) != NULL) {
      start = atoi(spec);
      dash++;
      if (*dash != 0)
	stop = atoi(dash);
    } else {
      start = stop = atoi(spec);
    }
    length = file_size(fd);
    if (start == 0 || stop == 0 || stop < start ||
	stop > length/NOV_BLOCK_SIZE)
      message(client, 434, "Invalid specification");
    else 
      output_overview_lines(client, fd, start, stop);
    close(fd);
  }
}

int com_over(FILE *client, char **args) {
  if (selected_group == NULL) {
    message(client, 412, "Not in a newsgroup");
  } else if (*args == NULL) {
    message(client, 413, "Name a message");
  } else {
    output_overview(client, *args);
  }

  return 0;
}
