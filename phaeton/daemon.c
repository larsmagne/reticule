#include <stdlib.h>
#include <ctype.h>
#include <netdb.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <stdio.h>
#include <time.h>
#include <signal.h>
#include <string.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <getopt.h>

#include "config.h"
#include "phaeton.h"
#include <util.h>

int server_socket = 0;

union sock {
  struct sockaddr s;
  struct sockaddr_in i;
};

#define BUFFER_SIZE 4096
#define MAX_ITEMS 256

void handle_client(int fd) {
  char buffer[BUFFER_SIZE];
  char *expression[MAX_ITEMS];
  char *s;
  int nitems = 0;

  int i = 0;

  while (read(fd, buffer+i, 1) == 1 &&
	 *(buffer+i) != '\n' &&
	 i++ < BUFFER_SIZE)
    ;
  if (*(buffer+i) == '\n')
    *(buffer+i+1) = 0;

  printf("Got %s", buffer);

  s = strtok(buffer, " \n");

  while (s && nitems < MAX_ITEMS) {
    expression[nitems++] = s;
    s = strtok(NULL, " \n");
  }
    
  expression[nitems] = NULL;

  if (nitems >= 1) {
    if (!strcmp(expression[0], "search")) {
      printf("Searching...\n");
    } else if (!strcmp(expression[0], "index")) {
    } 
  }

  close(fd);

  printf("Connection closed\n");
  exit(0);
}

void start_server(int port) {
  int wsd;
  int addlen;
  struct sockaddr_in sin, caddr;
  int nitems = 0;
  static int so_reuseaddr = 1;

  if ((server_socket = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    perror("No socket");
    exit(1);
  }

  setsockopt(server_socket, SOL_SOCKET, SO_REUSEADDR, &so_reuseaddr, 
	     sizeof(so_reuseaddr));

  sin.sin_family = AF_INET;
  sin.sin_port = htons(port);
  sin.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(server_socket, (struct sockaddr*)&sin, sizeof(sin)) == -1) {
    perror("Bind");
    exit(1);
  }

  if (listen(server_socket, 120) == -1) {
    perror("Bad listen");
    exit(1);
  }

  printf("Accepting (spool %s, Reticule %s)...\n",
	 news_spool, reticule_home);

  while (1) {
    nitems = 0;
    wsd = accept(server_socket, (struct sockaddr*)&caddr, &addlen);

    if (fork()) {
      close(wsd);
    } else {
      handle_client(wsd);
    }
  }
}

