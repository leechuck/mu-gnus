# Makefile for the mail assistant project

.PHONY: all test test-c test-python test-elisp test-llm install clean test-mail-process

# Variables
CC = gcc
CFLAGS = -Wall -Wextra -std=c99
LDFLAGS = 
SRCDIR = src/c
BUILDDIR = bin
INSTALLDIR = $(HOME)/.local/bin
PYTHONDIR = src/python

# Discover C source files
SOURCES = $(wildcard $(SRCDIR)/*.c)
# Separate mail-db from other targets
MAIL_DB_SRC = $(SRCDIR)/mail-db.c
MAIL_PROCESS_SRC = $(SRCDIR)/mail-process.c
CONFIG_SRC = $(SRCDIR)/config.c
CONFIG_OBJ = $(BUILDDIR)/config.o
OTHER_SOURCES = $(filter-out $(MAIL_DB_SRC) $(MAIL_PROCESS_SRC) $(CONFIG_SRC),$(SOURCES))
OTHER_TARGETS = $(patsubst $(SRCDIR)/%.c,$(BUILDDIR)/%,$(OTHER_SOURCES))

# Default target
all: $(BUILDDIR) $(BUILDDIR)/mail-db $(BUILDDIR)/mail-process $(OTHER_TARGETS)

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

# Rule for config object
$(CONFIG_OBJ): $(CONFIG_SRC) $(SRCDIR)/config.h
	$(CC) $(CFLAGS) -c -o $@ $(CONFIG_SRC)

# Special rule for mail-db which needs SQLite3
$(BUILDDIR)/mail-db: $(MAIL_DB_SRC) $(CONFIG_OBJ)
	$(CC) $(CFLAGS) -o $@ $(MAIL_DB_SRC) $(CONFIG_OBJ) -lsqlite3

# Rule for mail-process
$(BUILDDIR)/mail-process: $(MAIL_PROCESS_SRC) $(CONFIG_OBJ)
	$(CC) $(CFLAGS) -o $@ $(MAIL_PROCESS_SRC) $(CONFIG_OBJ)

# Generic rule for other C programs
$(BUILDDIR)/mail-extract: $(SRCDIR)/mail-extract.c
	$(CC) $(CFLAGS) -o $@ $<

# Test targets
test: test-c test-python test-elisp test-llm
	@echo "All tests finished."

test-c: all
	@echo "Running C tests..."
	@if [ -f tests/c/run_tests.sh ]; then chmod +x tests/c/run_tests.sh && ./tests/c/run_tests.sh; fi
	@if [ -f tests/c/test_mail_db.sh ]; then chmod +x tests/c/test_mail_db.sh && ./tests/c/test_mail_db.sh; fi
	@if [ -f tests/c/test_mail_process.sh ]; then chmod +x tests/c/test_mail_process.sh && ./tests/c/test_mail_process.sh; fi

test-python:
	@echo "Running Python tests (non-LLM)..."
	@if [ -f tests/python/test_mail_to_org.py ]; then cd tests/python && python3 test_mail_to_org.py; fi

test-llm:
	@echo "Running Python LLM tests..."
	@if [ -f tests/python/test_new_classify.py ]; then cd tests/python && python3 test_new_classify.py; fi

test-elisp:
	@echo "Running Elisp tests..."
	@if [ -f tests/elisp/test_mu_gnus.el ]; then \
		if emacs -batch -l ert -l tests/elisp/test_mu_gnus.el -f ert-run-tests-batch-and-exit; then \
			echo "OK"; \
		else \
			exit 1; \
		fi; \
	fi

# Test mail-process specifically (quick test)
test-mail-process: $(BUILDDIR)/mail-process $(BUILDDIR)/mail-db
	@echo "Running quick mail-process test..."
	@# Initialize test database
	@rm -f /tmp/test_mail_process.db
	@$(BUILDDIR)/mail-db init /tmp/test_mail_process.db
	@# Test with sample email
	@if [ -f tests/c/sample_email.txt ]; then \
		echo "Testing classification and database storage..."; \
		MAIL_DB_PATH=/tmp/test_mail_process.db MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo automated" \
		$(BUILDDIR)/mail-process < tests/c/sample_email.txt > /tmp/test_output.txt; \
		EXIT_CODE=$$?; \
		echo "Exit code: $$EXIT_CODE (3=automated)"; \
		if grep -q "^X-Label: automated" /tmp/test_output.txt; then \
			echo "✓ X-Label header added correctly"; \
		else \
			echo "✗ X-Label header not found"; \
			exit 1; \
		fi; \
		echo "Checking database entry..."; \
		$(BUILDDIR)/mail-db query --db /tmp/test_mail_process.db > /tmp/test_db_output.txt 2>/dev/null; \
		if grep -q "message_id" /tmp/test_db_output.txt; then \
			echo "✓ Email stored in database"; \
		else \
			echo "✗ Email not found in database"; \
			exit 1; \
		fi; \
	else \
		echo "Warning: tests/c/sample_email.txt not found, skipping mail-process test"; \
	fi
	@rm -f /tmp/test_mail_process.db /tmp/test_output.txt /tmp/test_db_output.txt
	@echo "Quick mail-process test completed."

# Install target
install: all
	mkdir -p $(INSTALLDIR)
	if [ -f $(BUILDDIR)/mail-db ]; then cp $(BUILDDIR)/mail-db $(INSTALLDIR)/; fi
	if [ -f $(BUILDDIR)/mail-extract ]; then cp $(BUILDDIR)/mail-extract $(INSTALLDIR)/; fi
	if [ -f $(BUILDDIR)/mail-process ]; then cp $(BUILDDIR)/mail-process $(INSTALLDIR)/; fi
	if [ -f $(PYTHONDIR)/mail-classify.py ]; then \
		cp $(PYTHONDIR)/mail-classify.py $(INSTALLDIR)/; \
		chmod +x $(INSTALLDIR)/mail-classify.py; \
	fi
	if [ -f $(PYTHONDIR)/mail-to-org.py ]; then \
		cp $(PYTHONDIR)/mail-to-org.py $(INSTALLDIR)/; \
		chmod +x $(INSTALLDIR)/mail-to-org.py; \
	fi
	@echo "Installation complete. Binaries installed to $(INSTALLDIR)"
	@echo "Make sure $(INSTALLDIR) is in your PATH."

# Clean target
clean:
	rm -rf $(BUILDDIR)
	find . -name "*.elc" -delete
	find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
	find . -name "*.pyc" -delete
	rm -f /tmp/test_mail.db /tmp/test_mail_process.db
	rm -f /tmp/test_output.txt /tmp/test_db_output.txt
	rm -f /tmp/test_email*.txt /tmp/test_output_*.txt
