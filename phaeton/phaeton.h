#ifndef PHAETON_C
#define PHAETON_C

#define VERSION "0.1"
#define MAX_GROUP_NAME_LENGTH 1024
#define MAX_GROUPS 16000

extern int server_port;
extern int server_socket;
extern char *version;
extern char *host_name;
extern int active_file;
extern int max_group_id;
extern char *selected_group;
extern char *client_address;

void start_server(int port);
void message(FILE *client, int code, char *message);
void open_active_file();
int select_group(char *group);
int message_present(int article);
int selectedp();
void tputs(char *string, FILE *client);

typedef struct {
  char *address;
  int groups;
  int articles;
  int bytes;
  int posts;
  int rejected;
  time_t logon;
  int selected_group;
} client_t;

extern client_t client;
#endif
