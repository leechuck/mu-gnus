#include "config.h"
#include <ctype.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>

// Trim whitespace from both ends of a string
static char* trim(char* str) {
    char* start = str;
    char* end;
    
    // Trim leading space
    while (isspace((unsigned char)*start)) start++;
    
    if (*start == 0) return start;
    
    // Trim trailing space
    end = start + strlen(start) - 1;
    while (end > start && isspace((unsigned char)*end)) end--;
    
    // Write new null terminator
    end[1] = '\0';
    
    return start;
}

// Expand ~ to home directory
char* expand_path(const char* path) {
    static char expanded[MAX_PATH_SIZE];
    
    if (path == NULL || path[0] == '\0') {
        return NULL;
    }
    
    if (path[0] == '~') {
        const char* home = getenv("HOME");
        if (!home) {
            struct passwd* pw = getpwuid(getuid());
            if (pw) {
                home = pw->pw_dir;
            }
        }
        if (home) {
            snprintf(expanded, sizeof(expanded), "%s%s", home, path + 1);
            return expanded;
        }
    }
    
    strncpy(expanded, path, sizeof(expanded) - 1);
    expanded[sizeof(expanded) - 1] = '\0';
    return expanded;
}

// Get the default config file path
char* get_config_path(void) {
    static char config_path[MAX_PATH_SIZE];
    
    // First check environment variable
    const char* env_path = getenv("MAIL_CONFIG_PATH");
    if (env_path) {
        return expand_path(env_path);
    }
    
    // Check for config.ini in current directory
    if (access("config.ini", F_OK) == 0) {
        return "config.ini";
    }
    
    // Check for ~/.config/mail-assistant/config.ini
    const char* home = getenv("HOME");
    if (!home) {
        struct passwd* pw = getpwuid(getuid());
        if (pw) {
            home = pw->pw_dir;
        }
    }
    
    if (home) {
        snprintf(config_path, sizeof(config_path), "%s/.config/mail-assistant/config.ini", home);
        if (access(config_path, F_OK) == 0) {
            return config_path;
        }
    }
    
    // Default to config.ini in current directory
    return "config.ini";
}

// Parse a configuration line
static void parse_config_line(Config* config, const char* section, const char* key, const char* value) {
    if (strcmp(section, "paths") == 0) {
        if (strcmp(key, "database") == 0) {
            char* expanded = expand_path(value);
            if (expanded) {
                strncpy(config->db_path, expanded, sizeof(config->db_path) - 1);
                config->db_path[sizeof(config->db_path) - 1] = '\0';
            }
        } else if (strcmp(key, "prompt_file") == 0) {
            char* expanded = expand_path(value);
            if (expanded) {
                strncpy(config->prompt_file, expanded, sizeof(config->prompt_file) - 1);
                config->prompt_file[sizeof(config->prompt_file) - 1] = '\0';
            }
        } else if (strcmp(key, "org_template") == 0) {
            char* expanded = expand_path(value);
            if (expanded) {
                strncpy(config->org_template, expanded, sizeof(config->org_template) - 1);
                config->org_template[sizeof(config->org_template) - 1] = '\0';
            }
        }
    } else if (strcmp(section, "llm") == 0) {
        if (strcmp(key, "type") == 0) {
            strncpy(config->llm_type, value, sizeof(config->llm_type) - 1);
            config->llm_type[sizeof(config->llm_type) - 1] = '\0';
        } else if (strcmp(key, "command") == 0) {
            strncpy(config->llm_cmd, value, sizeof(config->llm_cmd) - 1);
            config->llm_cmd[sizeof(config->llm_cmd) - 1] = '\0';
        } else if (strcmp(key, "model") == 0) {
            strncpy(config->llm_model, value, sizeof(config->llm_model) - 1);
            config->llm_model[sizeof(config->llm_model) - 1] = '\0';
        } else if (strcmp(key, "api_key") == 0) {
            strncpy(config->llm_api_key, value, sizeof(config->llm_api_key) - 1);
            config->llm_api_key[sizeof(config->llm_api_key) - 1] = '\0';
        } else if (strcmp(key, "api_url") == 0) {
            strncpy(config->llm_api_url, value, sizeof(config->llm_api_url) - 1);
            config->llm_api_url[sizeof(config->llm_api_url) - 1] = '\0';
        }
    }
}

// Load configuration from file
Config* load_config(const char* config_path) {
    Config* config = calloc(1, sizeof(Config));
    if (!config) {
        return NULL;
    }
    
    // Set defaults
    strncpy(config->db_path, "~/.mail.db", sizeof(config->db_path) - 1);
    strncpy(config->llm_type, "cmd", sizeof(config->llm_type) - 1);
    strncpy(config->llm_cmd, "echo automated", sizeof(config->llm_cmd) - 1);
    strncpy(config->prompt_file, "prompts/classify.txt", sizeof(config->prompt_file) - 1);
    
    // If no config path specified, try to find one
    if (!config_path) {
        config_path = get_config_path();
    }
    
    FILE* fp = fopen(config_path, "r");
    if (!fp) {
        // Config file not found, use defaults
        return config;
    }
    
    char line[MAX_LINE_SIZE];
    char current_section[MAX_VALUE_SIZE] = "";
    
    while (fgets(line, sizeof(line), fp)) {
        char* trimmed = trim(line);
        
        // Skip empty lines and comments
        if (trimmed[0] == '\0' || trimmed[0] == '#' || trimmed[0] == ';') {
            continue;
        }
        
        // Check for section header
        if (trimmed[0] == '[') {
            char* end = strchr(trimmed, ']');
            if (end) {
                *end = '\0';
                strncpy(current_section, trimmed + 1, sizeof(current_section) - 1);
                current_section[sizeof(current_section) - 1] = '\0';
            }
            continue;
        }
        
        // Parse key=value pairs
        char* equals = strchr(trimmed, '=');
        if (equals) {
            *equals = '\0';
            char* key = trim(trimmed);
            char* value = trim(equals + 1);
            
            // Remove quotes from value if present
            if (value[0] == '"' && value[strlen(value) - 1] == '"') {
                value[strlen(value) - 1] = '\0';
                value++;
            }
            
            parse_config_line(config, current_section, key, value);
        }
    }
    
    fclose(fp);
    return config;
}

// Free configuration structure
void free_config(Config* config) {
    if (config) {
        free(config);
    }
}
