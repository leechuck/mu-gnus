#ifndef CONFIG_H
#define CONFIG_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_PATH_SIZE 4096
#define MAX_LINE_SIZE 1024
#define MAX_VALUE_SIZE 512

// Configuration structure
typedef struct {
    char db_path[MAX_PATH_SIZE];
    char llm_type[MAX_VALUE_SIZE];
    char llm_cmd[MAX_VALUE_SIZE];
    char llm_model[MAX_VALUE_SIZE];
    char llm_api_key[MAX_VALUE_SIZE];
    char llm_api_url[MAX_VALUE_SIZE];
    char prompt_file[MAX_PATH_SIZE];
    char org_template[MAX_PATH_SIZE];
} Config;

// Function declarations
Config* load_config(const char* config_path);
void free_config(Config* config);
char* get_config_path(void);
char* expand_path(const char* path);

#endif // CONFIG_H
