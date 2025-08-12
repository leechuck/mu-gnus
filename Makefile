# Makefile for the mail assistant project

.PHONY: all test install clean

# Variables
CC = gcc
CFLAGS = -Wall -Wextra -std=c99
LDFLAGS = 
SRCDIR = src/c
BUILDDIR = bin
INSTALLDIR = $(HOME)/.local/bin

# Discover C source files
SOURCES = $(wildcard $(SRCDIR)/*.c)
# Separate mail-db from other targets
MAIL_DB_SRC = $(SRCDIR)/mail-db.c
OTHER_SOURCES = $(filter-out $(MAIL_DB_SRC),$(SOURCES))
OTHER_TARGETS = $(patsubst $(SRCDIR)/%.c,$(BUILDDIR)/%,$(OTHER_SOURCES))

# Default target
all: $(BUILDDIR) $(BUILDDIR)/mail-db $(OTHER_TARGETS)

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

# Special rule for mail-db which needs SQLite3
$(BUILDDIR)/mail-db: $(SRCDIR)/mail-db.c
	$(CC) $(CFLAGS) -o $@ $< -lsqlite3

# Generic rule for other C programs
$(BUILDDIR)/mail-extract: $(SRCDIR)/mail-extract.c
	$(CC) $(CFLAGS) -o $@ $<

# Test target
test: all
	@echo "Running C tests..."
	@if [ -f tests/c/run_tests.sh ]; then chmod +x tests/c/run_tests.sh && ./tests/c/run_tests.sh; fi
	@if [ -f tests/c/test_mail_db.sh ]; then chmod +x tests/c/test_mail_db.sh && ./tests/c/test_mail_db.sh; fi
	@echo "Running Python tests..."
	@if [ -f tests/python/test_classify.py ]; then cd tests/python && python3 test_classify.py; fi
	@echo "Running Elisp tests..."
	@# TODO: Add commands to run Elisp tests from tests/elisp/
	@echo "Tests finished."

# Install target
install: all
	mkdir -p $(INSTALLDIR)
	if [ -f $(BUILDDIR)/mail-db ]; then cp $(BUILDDIR)/mail-db $(INSTALLDIR)/; fi
	if [ -f $(BUILDDIR)/mail-extract ]; then cp $(BUILDDIR)/mail-extract $(INSTALLDIR)/; fi

# Clean target
clean:
	rm -rf $(BUILDDIR)
	find . -name "*.elc" -delete
	find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
	find . -name "*.pyc" -delete
	rm -f /tmp/test_mail.db
