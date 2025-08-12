#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>
#include <time.h>
#include <getopt.h>

#define MAX_QUERY_SIZE 4096
#define MAX_PATH_SIZE 1024

typedef struct {
    char *message_id;
    char *from_addr;
    char *subject;
    char *classification;
    char *urgency;
    char *sender_type;
    char *classification_json;
    int needs_reply;
    int replied;
    time_t date;
} EmailRecord;

void print_usage(const char *prog_name) {
    fprintf(stderr, "Usage:\n");
    fprintf(stderr, "  %s init <database_path>\n", prog_name);
    fprintf(stderr, "  %s add <message-id> --from <addr> --subject <subj> [--classification <class>] [--needs-reply] [--date <timestamp>] [--db <path>]\n", prog_name);
    fprintf(stderr, "  %s query [--needs-reply] [--not-replied] [--from <addr>] [--classification <class>] [--db <path>]\n", prog_name);
    fprintf(stderr, "  %s update <message-id> [--replied 0|1] [--needs-reply 0|1] [--classification <class>] [--db <path>]\n", prog_name);
    fprintf(stderr, "\nIf --db is not specified, uses MAIL_DB_PATH environment variable or ~/.mail.db\n");
}

void print_json_string(const char *str) {
    if (!str) {
        printf("null");
        return;
    }
    printf("\"");
    for (const char *p = str; *p; p++) {
        switch (*p) {
            case '"':
                printf("\\\"");
                break;
            case '\\':
                printf("\\\\");
                break;
            case '\b':
                printf("\\b");
                break;
            case '\f':
                printf("\\f");
                break;
            case '\n':
                printf("\\n");
                break;
            case '\r':
                printf("\\r");
                break;
            case '\t':
                printf("\\t");
                break;
            default:
                if (*p < 0x20) {
                    printf("\\u%04x", *p);
                } else {
                    putchar(*p);
                }
        }
    }
    printf("\"");
}

char* get_db_path(const char *explicit_path) {
    static char expanded_path[MAX_PATH_SIZE];
    
    const char *db_path = explicit_path;
    if (!db_path) {
        db_path = getenv("MAIL_DB_PATH");
    }
    if (!db_path) {
        db_path = "~/.mail.db";
    }
    
    // Expand tilde if present
    if (db_path[0] == '~') {
        const char *home = getenv("HOME");
        if (home) {
            snprintf(expanded_path, sizeof(expanded_path), "%s%s", home, db_path + 1);
            return expanded_path;
        }
    }
    
    // Copy to static buffer if not already there
    if (db_path != expanded_path) {
        strncpy(expanded_path, db_path, MAX_PATH_SIZE - 1);
        expanded_path[MAX_PATH_SIZE - 1] = '\0';
    }
    
    return expanded_path;
}

int init_database(const char *db_path) {
    sqlite3 *db;
    char *err_msg = NULL;
    
    int rc = sqlite3_open(db_path, &db);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return 1;
    }
    
    const char *sql = "CREATE TABLE IF NOT EXISTS emails ("
                     "message_id TEXT PRIMARY KEY,"
                     "date INTEGER,"
                     "from_addr TEXT,"
                     "subject TEXT,"
                     "classification TEXT,"
                     "needs_reply INTEGER,"
                     "replied INTEGER,"
                     "urgency TEXT,"
                     "sender_type TEXT,"
                     "classification_json TEXT"
                     ");";
    
    rc = sqlite3_exec(db, sql, 0, 0, &err_msg);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", err_msg);
        sqlite3_free(err_msg);
        sqlite3_close(db);
        return 1;
    }
    
    // Create indexes for common queries
    const char *index_sql[] = {
        "CREATE INDEX IF NOT EXISTS idx_needs_reply ON emails(needs_reply);",
        "CREATE INDEX IF NOT EXISTS idx_replied ON emails(replied);",
        "CREATE INDEX IF NOT EXISTS idx_from_addr ON emails(from_addr);",
        "CREATE INDEX IF NOT EXISTS idx_classification ON emails(classification);",
        "CREATE INDEX IF NOT EXISTS idx_date ON emails(date);",
        "CREATE INDEX IF NOT EXISTS idx_urgency ON emails(urgency);",
        "CREATE INDEX IF NOT EXISTS idx_sender_type ON emails(sender_type);",
        NULL
    };
    
    for (int i = 0; index_sql[i] != NULL; i++) {
        rc = sqlite3_exec(db, index_sql[i], 0, 0, &err_msg);
        if (rc != SQLITE_OK) {
            fprintf(stderr, "SQL error creating index: %s\n", err_msg);
            sqlite3_free(err_msg);
        }
    }
    
    sqlite3_close(db);
    printf("Database initialized successfully at %s\n", db_path);
    return 0;
}

int add_email(const char *db_path, EmailRecord *email) {
    sqlite3 *db;
    sqlite3_stmt *stmt;
    
    int rc = sqlite3_open(db_path, &db);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
        return 1;
    }
    
    const char *sql = "INSERT INTO emails (message_id, date, from_addr, subject, classification, needs_reply, replied, urgency, sender_type, classification_json) "
                     "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);";
    
    rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "Failed to prepare statement: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return 1;
    }
    
    sqlite3_bind_text(stmt, 1, email->message_id, -1, SQLITE_STATIC);
    sqlite3_bind_int64(stmt, 2, email->date);
    sqlite3_bind_text(stmt, 3, email->from_addr, -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 4, email->subject, -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 5, email->classification, -1, SQLITE_STATIC);
    sqlite3_bind_int(stmt, 6, email->needs_reply);
    sqlite3_bind_int(stmt, 7, email->replied);
    sqlite3_bind_text(stmt, 8, email->urgency, -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 9, email->sender_type, -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 10, email->classification_json, -1, SQLITE_STATIC);
    
    rc = sqlite3_step(stmt);
    if (rc != SQLITE_DONE) {
        fprintf(stderr, "Execution failed: %s\n", sqlite3_errmsg(db));
        sqlite3_finalize(stmt);
        sqlite3_close(db);
        return 1;
    }
    
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    printf("Email added successfully: %s\n", email->message_id);
    return 0;
}

int query_emails(const char *db_path, int needs_reply, int not_replied, const char *from_addr, const char *classification) {
    sqlite3 *db;
    sqlite3_stmt *stmt;
    
    int rc = sqlite3_open(db_path, &db);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
        return 1;
    }
    
    char sql[MAX_QUERY_SIZE] = "SELECT message_id, date, from_addr, subject, classification, needs_reply, replied, urgency, sender_type, classification_json FROM emails WHERE 1=1";
    
    if (needs_reply >= 0) {
        strcat(sql, " AND needs_reply = 1");
    }
    if (not_replied >= 0) {
        strcat(sql, " AND replied = 0");
    }
    if (from_addr) {
        strcat(sql, " AND from_addr = ?");
    }
    if (classification) {
        strcat(sql, " AND classification = ?");
    }
    strcat(sql, " ORDER BY date DESC;");
    
    rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "Failed to prepare statement: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return 1;
    }
    
    int param_index = 1;
    if (from_addr) {
        sqlite3_bind_text(stmt, param_index++, from_addr, -1, SQLITE_STATIC);
    }
    if (classification) {
        sqlite3_bind_text(stmt, param_index++, classification, -1, SQLITE_STATIC);
    }
    
    printf("[\n");
    int first = 1;
    
    while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
        if (!first) {
            printf(",\n");
        }
        first = 0;
        
        printf("  {\n");
        printf("    \"message_id\": ");
        print_json_string((const char *)sqlite3_column_text(stmt, 0));
        printf(",\n");
        
        printf("    \"date\": %lld,\n", (long long)sqlite3_column_int64(stmt, 1));
        
        printf("    \"from_addr\": ");
        print_json_string((const char *)sqlite3_column_text(stmt, 2));
        printf(",\n");
        
        printf("    \"subject\": ");
        print_json_string((const char *)sqlite3_column_text(stmt, 3));
        printf(",\n");
        
        printf("    \"classification\": ");
        print_json_string((const char *)sqlite3_column_text(stmt, 4));
        printf(",\n");
        
        printf("    \"urgency\": ");
        print_json_string((const char *)sqlite3_column_text(stmt, 7));
        printf(",\n");

        printf("    \"sender_type\": ");
        print_json_string((const char *)sqlite3_column_text(stmt, 8));
        printf(",\n");

        printf("    \"classification_json\": ");
        print_json_string((const char *)sqlite3_column_text(stmt, 9));
        printf(",\n");

        printf("    \"needs_reply\": %s,\n", sqlite3_column_int(stmt, 5) ? "true" : "false");
        printf("    \"replied\": %s\n", sqlite3_column_int(stmt, 6) ? "true" : "false");
        printf("  }");
    }
    
    printf("\n]\n");
    
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    return 0;
}

int update_email(const char *db_path, const char *message_id, int replied, int needs_reply, const char *classification) {
    sqlite3 *db;
    sqlite3_stmt *stmt;
    
    int rc = sqlite3_open(db_path, &db);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(db));
        return 1;
    }
    
    char sql[MAX_QUERY_SIZE] = "UPDATE emails SET ";
    int has_update = 0;
    
    if (replied >= 0) {
        strcat(sql, "replied = ?");
        has_update = 1;
    }
    if (needs_reply >= 0) {
        if (has_update) strcat(sql, ", ");
        strcat(sql, "needs_reply = ?");
        has_update = 1;
    }
    if (classification) {
        if (has_update) strcat(sql, ", ");
        strcat(sql, "classification = ?");
        has_update = 1;
    }
    
    if (!has_update) {
        fprintf(stderr, "No fields to update\n");
        sqlite3_close(db);
        return 1;
    }
    
    strcat(sql, " WHERE message_id = ?;");
    
    rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "Failed to prepare statement: %s\n", sqlite3_errmsg(db));
        sqlite3_close(db);
        return 1;
    }
    
    int param_index = 1;
    if (replied >= 0) {
        sqlite3_bind_int(stmt, param_index++, replied);
    }
    if (needs_reply >= 0) {
        sqlite3_bind_int(stmt, param_index++, needs_reply);
    }
    if (classification) {
        sqlite3_bind_text(stmt, param_index++, classification, -1, SQLITE_STATIC);
    }
    sqlite3_bind_text(stmt, param_index, message_id, -1, SQLITE_STATIC);
    
    rc = sqlite3_step(stmt);
    if (rc != SQLITE_DONE) {
        fprintf(stderr, "Execution failed: %s\n", sqlite3_errmsg(db));
        sqlite3_finalize(stmt);
        sqlite3_close(db);
        return 1;
    }
    
    int changes = sqlite3_changes(db);
    if (changes == 0) {
        fprintf(stderr, "No email found with message_id: %s\n", message_id);
        sqlite3_finalize(stmt);
        sqlite3_close(db);
        return 1;
    }
    
    sqlite3_finalize(stmt);
    sqlite3_close(db);
    printf("Email updated successfully: %s\n", message_id);
    return 0;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        print_usage(argv[0]);
        return 1;
    }
    
    if (strcmp(argv[1], "init") == 0) {
        if (argc < 3) {
            fprintf(stderr, "Error: database path required\n");
            print_usage(argv[0]);
            return 1;
        }
        return init_database(argv[2]);
    }
    else if (strcmp(argv[1], "add") == 0) {
        if (argc < 3) {
            fprintf(stderr, "Error: message-id required\n");
            print_usage(argv[0]);
            return 1;
        }
        
        EmailRecord email = {0};
        email.message_id = argv[2];
        email.date = time(NULL);
        email.replied = 0;
        email.needs_reply = 0;
        
        char *explicit_db_path = NULL;
        
        static struct option long_options[] = {
            {"from", required_argument, 0, 'f'},
            {"subject", required_argument, 0, 's'},
            {"classification", required_argument, 0, 'c'},
            {"needs-reply", no_argument, 0, 'n'},
            {"date", required_argument, 0, 'd'},
            {"db", required_argument, 0, 'b'},
            {"urgency", required_argument, 0, 'u'},
            {"sender-type", required_argument, 0, 't'},
            {"classification-json", required_argument, 0, 'j'},
            {0, 0, 0, 0}
        };
        
        int option_index = 0;
        int c;
        optind = 3; // Start after "add <message-id>"
        
        while ((c = getopt_long(argc, argv, "f:s:c:nd:b:u:t:j:", long_options, &option_index)) != -1) {
            switch (c) {
                case 'f':
                    email.from_addr = optarg;
                    break;
                case 's':
                    email.subject = optarg;
                    break;
                case 'c':
                    email.classification = optarg;
                    break;
                case 'n':
                    email.needs_reply = 1;
                    break;
                case 'd':
                    email.date = atol(optarg);
                    break;
                case 'b':
                    explicit_db_path = optarg;
                    break;
                case 'u':
                    email.urgency = optarg;
                    break;
                case 't':
                    email.sender_type = optarg;
                    break;
                case 'j':
                    email.classification_json = optarg;
                    break;
                default:
                    print_usage(argv[0]);
                    return 1;
            }
        }
        
        return add_email(get_db_path(explicit_db_path), &email);
    }
    else if (strcmp(argv[1], "query") == 0) {
        int needs_reply = -1;
        int not_replied = -1;
        char *from_addr = NULL;
        char *classification = NULL;
        char *explicit_db_path = NULL;
        
        static struct option long_options[] = {
            {"needs-reply", no_argument, 0, 'n'},
            {"not-replied", no_argument, 0, 'r'},
            {"from", required_argument, 0, 'f'},
            {"classification", required_argument, 0, 'c'},
            {"db", required_argument, 0, 'b'},
            {0, 0, 0, 0}
        };
        
        int option_index = 0;
        int c;
        optind = 2; // Start after "query"
        
        while ((c = getopt_long(argc, argv, "nrf:c:b:", long_options, &option_index)) != -1) {
            switch (c) {
                case 'n':
                    needs_reply = 1;
                    break;
                case 'r':
                    not_replied = 1;
                    break;
                case 'f':
                    from_addr = optarg;
                    break;
                case 'c':
                    classification = optarg;
                    break;
                case 'b':
                    explicit_db_path = optarg;
                    break;
                default:
                    print_usage(argv[0]);
                    return 1;
            }
        }
        
        return query_emails(get_db_path(explicit_db_path), needs_reply, not_replied, from_addr, classification);
    }
    else if (strcmp(argv[1], "update") == 0) {
        if (argc < 3) {
            fprintf(stderr, "Error: message-id required\n");
            print_usage(argv[0]);
            return 1;
        }
        
        char *message_id = argv[2];
        int replied = -1;
        int needs_reply = -1;
        char *classification = NULL;
        char *explicit_db_path = NULL;
        
        static struct option long_options[] = {
            {"replied", required_argument, 0, 'r'},
            {"needs-reply", required_argument, 0, 'n'},
            {"classification", required_argument, 0, 'c'},
            {"db", required_argument, 0, 'b'},
            {0, 0, 0, 0}
        };
        
        int option_index = 0;
        int c;
        optind = 3; // Start after "update <message-id>"
        
        while ((c = getopt_long(argc, argv, "r:n:c:b:", long_options, &option_index)) != -1) {
            switch (c) {
                case 'r':
                    replied = atoi(optarg);
                    break;
                case 'n':
                    needs_reply = atoi(optarg);
                    break;
                case 'c':
                    classification = optarg;
                    break;
                case 'b':
                    explicit_db_path = optarg;
                    break;
                default:
                    print_usage(argv[0]);
                    return 1;
            }
        }
        
        return update_email(get_db_path(explicit_db_path), message_id, replied, needs_reply, classification);
    }
    else {
        fprintf(stderr, "Unknown command: %s\n", argv[1]);
        print_usage(argv[0]);
        return 1;
    }
    
    return 0;
}
