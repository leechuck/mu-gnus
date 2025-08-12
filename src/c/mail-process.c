#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/wait.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>

#define MAX_EMAIL_SIZE (10 * 1024 * 1024) // 10MB
#define MAX_LINE_LENGTH 4096

// Helper to trim whitespace from start and end of a string
char* trim_whitespace(char *str) {
    if (!str) return NULL;
    char *end;
    while (isspace((unsigned char)*str)) str++;
    if (*str == 0) return str;
    end = str + strlen(str) - 1;
    while (end > str && isspace((unsigned char)*end)) end--;
    end[1] = '\0';
    return str;
}

// Extracts a header value from the email content.
// Returns a dynamically allocated string, or NULL.
// Handles multi-line headers.
char* extract_header(const char* email_content, const char* header_name) {
    const char *headers_end = strstr(email_content, "\n\n");
    if (!headers_end) {
        headers_end = strstr(email_content, "\r\n\r\n");
    }
    size_t search_len = headers_end ? (size_t)(headers_end - email_content) : strlen(email_content);

    const char *p = email_content;
    size_t header_len = strlen(header_name);
    char line[MAX_LINE_LENGTH];
    char *value = NULL;
    size_t value_size = 0;
    int found = 0;

    while (p && *p && (size_t)(p - email_content) < search_len) {
        const char *next_line = strchr(p, '\n');
        size_t line_len = next_line ? (size_t)(next_line - p) : strlen(p);
        
        if (line_len >= MAX_LINE_LENGTH) line_len = MAX_LINE_LENGTH - 1;
        
        strncpy(line, p, line_len);
        line[line_len] = '\0';

        if (!found && strncasecmp(line, header_name, header_len) == 0 && line[header_len] == ':') {
            found = 1;
            char *current_value = trim_whitespace(line + header_len + 1);
            size_t current_len = strlen(current_value);
            value = malloc(current_len + 1);
            if (!value) return NULL;
            strcpy(value, current_value);
            value_size = current_len + 1;
        } else if (found && (line[0] == ' ' || line[0] == '\t')) {
            // Folded line
            char *current_value = trim_whitespace(line);
            size_t current_len = strlen(current_value);
            char *new_value = realloc(value, value_size + 1 + current_len); // +1 for space
            if (!new_value) {
                free(value);
                return NULL;
            }
            value = new_value;
            strcat(value, " ");
            strcat(value, current_value);
            value_size += 1 + current_len;
        } else if (found) {
            // Next header, so we are done with the current one.
            break;
        }

        p = next_line ? next_line + 1 : NULL;
    }
    return value;
}

// Reads the entire email from stdin.
// Returns a dynamically allocated string.
char* read_email() {
    char *buffer = malloc(MAX_EMAIL_SIZE);
    if (!buffer) {
        perror("malloc for email buffer");
        return NULL;
    }
    size_t bytes_read = fread(buffer, 1, MAX_EMAIL_SIZE - 1, stdin);
    if (bytes_read == 0 && ferror(stdin)) {
        perror("fread from stdin");
        free(buffer);
        return NULL;
    }
    buffer[bytes_read] = '\0';
    return buffer;
}

// Calls mail-classify.py to get the classification.
// Returns a dynamically allocated string with the classification.
char* classify_email(const char* email_content) {
    char *classification = malloc(128);
    if (!classification) return NULL;
    strcpy(classification, "automated"); // Default

    char *python_script = "src/python/mail-classify.py";
    
    int to_child_pipe[2];
    int from_child_pipe[2];

    if (pipe(to_child_pipe) == -1 || pipe(from_child_pipe) == -1) {
        perror("pipe");
        return classification;
    }

    pid_t pid = fork();
    if (pid == -1) {
        perror("fork");
        close(to_child_pipe[0]); close(to_child_pipe[1]);
        close(from_child_pipe[0]); close(from_child_pipe[1]);
        return classification;
    }

    if (pid == 0) { // Child
        close(to_child_pipe[1]);
        dup2(to_child_pipe[0], STDIN_FILENO);
        close(to_child_pipe[0]);

        close(from_child_pipe[0]);
        dup2(from_child_pipe[1], STDOUT_FILENO);
        close(from_child_pipe[1]);

        int dev_null = open("/dev/null", O_WRONLY);
        if (dev_null != -1) {
            dup2(dev_null, STDERR_FILENO);
            close(dev_null);
        }

        execlp(python_script, python_script, NULL);
        perror("execlp mail-classify.py");
        exit(1);
    } else { // Parent
        close(to_child_pipe[0]);
        close(from_child_pipe[1]);

        write(to_child_pipe[1], email_content, strlen(email_content));
        close(to_child_pipe[1]);

        char buffer[128];
        ssize_t count = read(from_child_pipe[0], buffer, sizeof(buffer) - 1);
        if (count > 0) {
            buffer[count] = '\0';
            strncpy(classification, trim_whitespace(buffer), 127);
            classification[127] = '\0';
        }
        close(from_child_pipe[0]);

        waitpid(pid, NULL, 0);
    }

    return classification;
}

// Calls `mail-db` to add the email record.
void add_to_database(const char* message_id, const char* from, const char* subject, const char* classification, int needs_reply) {
    if (!message_id || strlen(message_id) == 0) return;

    char *argv[12];
    int argc = 0;

    argv[argc++] = "./bin/mail-db";
    argv[argc++] = "add";
    argv[argc++] = (char*)message_id;

    if (from) {
        argv[argc++] = "--from";
        argv[argc++] = (char*)from;
    }
    if (subject) {
        argv[argc++] = "--subject";
        argv[argc++] = (char*)subject;
    }
    if (classification) {
        argv[argc++] = "--classification";
        argv[argc++] = (char*)classification;
    }
    if (needs_reply) {
        argv[argc++] = "--needs-reply";
    }
    argv[argc] = NULL;

    pid_t pid = fork();
    if (pid == -1) {
        perror("fork for mail-db");
        return;
    }

    if (pid == 0) { // Child
        int dev_null = open("/dev/null", O_WRONLY);
        if (dev_null != -1) {
            dup2(dev_null, STDOUT_FILENO);
            dup2(dev_null, STDERR_FILENO);
            close(dev_null);
        }
        execvp(argv[0], argv);
        perror("execvp mail-db");
        exit(1);
    } else { // Parent
        waitpid(pid, NULL, 0);
    }
}

// Gets the exit code for a given classification.
int get_exit_code(const char* classification) {
    if (strcmp(classification, "important") == 0) return 0;
    if (strcmp(classification, "newsletter") == 0) return 1;
    if (strcmp(classification, "social") == 0) return 2;
    // "automated" and any other/fallback
    return 3;
}

int main() {
    char* email_content = read_email();
    if (!email_content) {
        return 3; // Default to automated/error
    }

    char* classification = classify_email(email_content);
    
    char* message_id_raw = extract_header(email_content, "Message-ID");
    char* message_id = NULL;
    if (message_id_raw) {
        char* start = strchr(message_id_raw, '<');
        if (start) {
            start++;
            char* end = strchr(start, '>');
            if (end) {
                *end = '\0';
            }
            message_id = strdup(start);
        } else {
            message_id = strdup(trim_whitespace(message_id_raw));
        }
        free(message_id_raw);
    }

    char* from = extract_header(email_content, "From");
    char* subject = extract_header(email_content, "Subject");
    char* existing_xlabel = extract_header(email_content, "X-Label");

    int needs_reply = (strcmp(classification, "important") == 0);
    add_to_database(message_id, from, subject, classification, needs_reply);

    if (!existing_xlabel) {
        printf("X-Label: %s\n", classification);
    }
    printf("%s", email_content);

    int exit_code = get_exit_code(classification);

    free(email_content);
    free(classification);
    free(message_id);
    free(from);
    free(subject);
    free(existing_xlabel);

    return exit_code;
}
