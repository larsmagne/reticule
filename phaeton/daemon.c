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
#include <glib.h>
#include <syslog.h>

#include "config.h"
#include "dispatch.h"
#include "phaeton.h"
#include "commands.h"
#include <util.h>

int server_socket = 0;

client_t client;

union sock {
  struct sockaddr s;
  struct sockaddr_in i;
};

#define BUFFER_SIZE 4096
#define MAX_ITEMS 256

dispatcher dispatchers[] = {
  {"group", com_group, {STRING, EOA}},
  {"article", com_article, {STRING, EOA}},
  {"mode", com_mode, {STRING, EOA}},
  {"quit", com_quit, {EOA}},
  {"list", com_list, {EOA}},
  {"help", com_help, {EOA}},
  {"over", com_over, {STRING, EOA}},
  {"xover", com_over, {STRING, EOA}},
  {NULL, NULL, {0}}  
};

int handle_command(FILE *client, char **command) {
  return dispatch(client, command, dispatchers);
}

void report_stats() {
  syslog(LOG_ALERT, "%s: stats groups:%d articles:%d bytes:%d time:%d\n",
	 client.address, client.groups, client.articles,
	 client.bytes, time(NULL) - client.logon);
}

void handle_client(int fd) {
  char buffer[BUFFER_SIZE];
  char *expression[MAX_ITEMS];
  char *s;
  int nitems = 0;
  int i = 0;
  int endp = 0, rlen;
  FILE *client;
  int result;
  char *ret;

  if (! (client = fdopen(fd, "r+"))) {
    printf("Couldn't fdopen\n");
    exit(-1);
  }

  com_hello(client, NULL);
  fflush(client);

  open_active_file();

  while (! endp) {
    i = 0;
    nitems = 0;

    while ((rlen = read(fd, buffer+i, 1)) == 1 &&
	   *(buffer+i) != '\n' &&
	   i++ < BUFFER_SIZE)
      ;

    if (rlen == 0)
      goto out;

    if ((ret = strchr(buffer, '\r')) != NULL)
      *ret = 0;

    if ((ret = strchr(buffer, '\n')) != NULL)
      *ret = 0;
      
    printf("Got '%s'\n", buffer);

    s = strtok(buffer, " \n");

    while (s && nitems < MAX_ITEMS) {
      expression[nitems++] = s;
      s = strtok(NULL, " \n");
    }
    
    expression[nitems] = NULL;

    if (nitems >= 1) {
      result = handle_command(client, expression);
      if (result < 0)
	message(client, 500, "Unknown command");
      else if (result > 0)
	endp = 1;
      fflush(client);
    }
  }

 out:

  fclose(client);
  close(fd);

  report_stats();

  printf("Connection closed\n");
  exit(0);
}

void start_server(int port) {
  int wsd;
  int addlen;
  struct sockaddr_in sin, caddr;
  int nitems = 0;
  static int so_reuseaddr = 1;
  struct hostent *host;

  if ((server_socket = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    perror("No socket");
    exit(1);
  }

  openlog("phaetond", LOG_PID, LOG_DAEMON);

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

  read_groups_file();

  printf("Accepting (spool %s)...\n", news_spool);

  while (1) {
    nitems = 0;
    wsd = accept(server_socket, (struct sockaddr*)&caddr, &addlen);

    if (fork()) {
      close(wsd);
    } else {
      getpeername(wsd, (struct sockaddr*)&caddr, &addlen);
      
      host = gethostbyaddr( (const void*)&caddr.sin_addr, 
			    sizeof(struct in_addr), 
			    AF_INET);

      bzero(&client, sizeof(client));

      if (host != NULL) 
	client.address = strdup(host->h_name);
      else
	client.address = strdup(inet_ntoa(caddr.sin_addr));
      client.logon = time(NULL);

      syslog(LOG_ALERT, "%s: connection", client.address);

      handle_client(wsd);
    }
  }
}

