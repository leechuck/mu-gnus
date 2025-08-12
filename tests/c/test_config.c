#define _DEFAULT_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include "../../src/c/config.h"

// Helper to create a test config file
void create_test_config(const char* path, const char* content) {
    FILE* f = fopen(path, "w");
    assert(f != NULL);
    fprintf(f, "%s", content);
    fclose(f);
}

void test_defaults_and_loading() {
    printf("Testing config loading and defaults...\n");
    
    // Test with non-existent file (should use defaults)
    Config* config_defaults = load_config("/tmp/non_existent_config.ini");
    assert(config_defaults != NULL);
    assert(strcmp(config_defaults->db_path, "~/.mail.db") == 0);
    assert(strcmp(config_defaults->llm_type, "cmd") == 0);
    assert(strcmp(config_defaults->llm_cmd, "echo automated") == 0);
    free_config(config_defaults);

    // Create a dummy config file
    const char* config_content = 
        "[paths]\n"
        "database = ~/.test.db\n"
        "prompt_file = /tmp/prompt.txt\n\n"
        "[llm]\n"
        "type = openai\n"
        "command = should_be_overwritten\n"
        "model = gpt-4\n";
    create_test_config("/tmp/test_config.ini", config_content);

    Config* config = load_config("/tmp/test_config.ini");
    assert(config != NULL);

    char* home = getenv("HOME");
    if (home) {
        char expected_db_path[MAX_PATH_SIZE];
        snprintf(expected_db_path, sizeof(expected_db_path), "%s/.test.db", home);
        assert(strcmp(config->db_path, expected_db_path) == 0);
    }
    assert(strcmp(config->prompt_file, "/tmp/prompt.txt") == 0);
    assert(strcmp(config->llm_type, "openai") == 0);
    assert(strcmp(config->llm_model, "gpt-4") == 0);
    assert(strcmp(config->llm_cmd, "should_be_overwritten") == 0);

    free_config(config);
    unlink("/tmp/test_config.ini");
    printf("PASS\n");
}

void test_expand_path() {
    printf("Testing expand_path...\n");
    char* home = getenv("HOME");
    if (home) {
        char expected_path[MAX_PATH_SIZE];
        snprintf(expected_path, sizeof(expected_path), "%s/test", home);
        assert(strcmp(expand_path("~/test"), expected_path) == 0);
    }
    assert(strcmp(expand_path("/tmp/test"), "/tmp/test") == 0);
    assert(expand_path(NULL) == NULL);
    assert(expand_path("") == NULL);
    printf("PASS\n");
}

void test_get_config_path() {
    printf("Testing get_config_path...\n");
    
    // Test with env var
    setenv("MAIL_CONFIG_PATH", "/tmp/env_config.ini", 1);
    assert(strcmp(get_config_path(), "/tmp/env_config.ini") == 0);
    unsetenv("MAIL_CONFIG_PATH");

    // Test with config.ini in current dir
    create_test_config("config.ini", "# test file");
    assert(strcmp(get_config_path(), "config.ini") == 0);
    unlink("config.ini");

    // Test fallback (should be "config.ini")
    assert(strcmp(get_config_path(), "config.ini") == 0);

    printf("PASS\n");
}

int main() {
    test_defaults_and_loading();
    test_expand_path();
    test_get_config_path();
    printf("\nAll config tests passed.\n");
    return 0;
}
