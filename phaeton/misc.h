#ifndef MISC_C
#define MISC_C

extern char *reticule_home;
extern char *spool_directory;

extern GHashTable *group_table;
extern char *reverse_group_table[];
extern int max_group_id;

char *group_id_to_name(int id);
int group_name_to_id(char *group);
void read_groups_file();
int isnumerical(char *string);
char *get_article_name(char *group, int article);
char *get_overview_name(char *group, char *extension);
char *reticule_file_name(char *name);
void read_groups_file(void);
char *get_group_directory(const char *group);
char *mrealloc(char *buffer, int old_size, int new_size);

#endif
