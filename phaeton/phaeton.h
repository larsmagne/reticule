#ifndef PHAETON_C
#define PHAETON_C

#define VERSION "0.1"
#define MAX_GROUP_NAME_LENGTH 1024
#define MAX_GROUPS 16000

extern char *reticule_home;
extern char *spool_directory;
extern int server_port;
extern int server_socket;
extern char *version;
extern char *host_name;
extern int active_file;
extern int max_group_id;
extern char *selected_group;

void start_server(int port);
void message(FILE *client, int code, char *message);
char *group_id_to_name(int id);
int group_name_to_id(char *group);
void read_groups_file();
void open_active_file();
int select_group(char *group);
int isnumerical(char *string);
char *get_article_name(char *group, int article);
int message_present(int article);
char *get_overview_name(char *group, char *extension);

#endif
