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
#include <util.h>

struct option long_options[] = {
  {"spool", 1, 0, 's'},
  {"reticule", 1, 0, 'i'},
  {"port", 1, 0, 'p'},
  {"help", 0, 0, 'h'},
  {0, 0, 0, 0}
};

void closedown(int i);

int parse_args(int argc, char **argv) {
  int option_index = 0, c;
  while (1) {
    c = getopt_long(argc, argv, "hs:r:p:", long_options, &option_index);
    if (c == -1)
      break;

    switch (c) {
    case 's':
      news_spool = optarg;
      break;
      
    case 'p':
      reticule_home = optarg;
      break;
      
    case 'p':
      server_port = atoi(optarg);
      break;
      
    case 'h':
      printf ("Usage: phaetond [--spool <directory>]\n");
      break;

    default:
      break;
    }
  }

  return optind;
}

int main(int argc, char **argv)
{

  dirn = parse_args(argc, argv);
  
  if (signal(SIGHUP, closedown) == SIG_ERR) {
    perror("Signal");
    exit(1);
  }

  if (signal(SIGINT, closedown) == SIG_ERR) {
    perror("Signal");
    exit(1);
  }

  /* Initialize key/data structures. */
  init();
  time(&start_time);

  closedown();

  exit(0);
}

void closedown(int i) {
 time_t now = time(NULL);

 flush();
 printf("Closed down at %s", ctime(&now));
 exit(0);
}
