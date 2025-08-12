#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LENGTH 4096
#define MAX_HEADERS 100

typedef struct {
    char *name;
    char *value;
} Header;

typedef struct {
    Header headers[MAX_HEADERS];
    int header_count;
    char *body;
} Email;

// Case-insensitive string comparison
int strcasecmp_custom(const char *s1, const char *s2) {
    while (*s1 && *s2) {
        if (tolower(*s1) != tolower(*s2)) {
            return tolower(*s1) - tolower(*s2);
        }
        s1++;
        s2++;
    }
    return tolower(*s1) - tolower(*s2);
}

// Trim leading and trailing whitespace
char *trim(char *str) {
    char *start = str;
    char *end;
    
    // Trim leading space
    while (isspace(*start)) start++;
    
    if (*start == 0) return start;
    
    // Trim trailing space
    end = start + strlen(start) - 1;
    while (end > start && isspace(*end)) end--;
    
    // Write new null terminator
    *(end + 1) = '\0';
    
    return start;
}

// Parse email from stdin
Email *parse_email() {
    Email *email = malloc(sizeof(Email));
    email->header_count = 0;
    email->body = NULL;
    
    char line[MAX_LINE_LENGTH];
    char current_header_name[MAX_LINE_LENGTH] = "";
    char current_header_value[MAX_LINE_LENGTH * 10] = "";
    int in_body = 0;
    char body_buffer[MAX_LINE_LENGTH * 100] = "";
    
    while (fgets(line, sizeof(line), stdin)) {
        if (!in_body) {
            // Check for end of headers (empty line)
            if (line[0] == '\n' || line[0] == '\r') {
                // Save last header if exists
                if (strlen(current_header_name) > 0 && email->header_count < MAX_HEADERS) {
                    email->headers[email->header_count].name = strdup(current_header_name);
                    email->headers[email->header_count].value = strdup(trim(current_header_value));
                    email->header_count++;
                }
                in_body = 1;
                continue;
            }
            
            // Check if this is a continuation line (starts with space or tab)
            if (line[0] == ' ' || line[0] == '\t') {
                // Continuation of previous header
                char *trimmed = trim(line);
                if (strlen(current_header_value) > 0) {
                    strcat(current_header_value, " ");
                }
                strcat(current_header_value, trimmed);
            } else {
                // New header line
                // Save previous header if exists
                if (strlen(current_header_name) > 0 && email->header_count < MAX_HEADERS) {
                    email->headers[email->header_count].name = strdup(current_header_name);
                    email->headers[email->header_count].value = strdup(trim(current_header_value));
                    email->header_count++;
                }
                
                // Parse new header
                char *colon = strchr(line, ':');
                if (colon) {
                    *colon = '\0';
                    strcpy(current_header_name, line);
                    strcpy(current_header_value, trim(colon + 1));
                }
            }
        } else {
            // In body
            strcat(body_buffer, line);
        }
    }
    
    // Save last header if we ended without a body
    if (!in_body && strlen(current_header_name) > 0 && email->header_count < MAX_HEADERS) {
        email->headers[email->header_count].name = strdup(current_header_name);
        email->headers[email->header_count].value = strdup(trim(current_header_value));
        email->header_count++;
    }
    
    if (strlen(body_buffer) > 0) {
        email->body = strdup(body_buffer);
    }
    
    return email;
}

// Find header by name (case-insensitive)
char *find_header(Email *email, const char *name) {
    for (int i = 0; i < email->header_count; i++) {
        if (strcasecmp_custom(email->headers[i].name, name) == 0) {
            return email->headers[i].value;
        }
    }
    return NULL;
}

// Print headers as JSON
void print_json(Email *email) {
    printf("{\n");
    for (int i = 0; i < email->header_count; i++) {
        printf("  \"%s\": \"%s\"", email->headers[i].name, email->headers[i].value);
        if (i < email->header_count - 1) {
            printf(",");
        }
        printf("\n");
    }
    printf("}\n");
}

// Free email structure
void free_email(Email *email) {
    for (int i = 0; i < email->header_count; i++) {
        free(email->headers[i].name);
        free(email->headers[i].value);
    }
    if (email->body) {
        free(email->body);
    }
    free(email);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s --header <name> | --body | --json\n", argv[0]);
        return 1;
    }
    
    Email *email = parse_email();
    
    if (strcmp(argv[1], "--header") == 0) {
        if (argc < 3) {
            fprintf(stderr, "Error: --header requires a header name\n");
            free_email(email);
            return 1;
        }
        char *value = find_header(email, argv[2]);
        if (value) {
            printf("%s\n", value);
        }
    } else if (strcmp(argv[1], "--body") == 0) {
        if (email->body) {
            printf("%s", email->body);
        }
    } else if (strcmp(argv[1], "--json") == 0) {
        print_json(email);
    } else {
        fprintf(stderr, "Unknown option: %s\n", argv[1]);
        free_email(email);
        return 1;
    }
    
    free_email(email);
    return 0;
}
