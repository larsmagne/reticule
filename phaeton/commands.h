#ifndef COMMANDS_H
#define COMMANDS_H

int com_group(FILE *client, char **args);
int com_article(FILE *client, char **args);
int com_hello(FILE *client, char **args);
int com_mode(FILE *client, char **args);
int com_quit(FILE *client, char **args);
int com_list(FILE *client, char **args);
int com_over(FILE *client, char **args);
int com_help(FILE *client, char **args);

#endif COMMANDS_H
