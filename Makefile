# Makefile for the mail assistant project

.PHONY: all test install clean

# Variables
CC = gcc
CFLAGS = -Wall -Wextra -std=c99
SRCDIR = src/c
BUILDDIR = bin
INSTALLDIR = $(HOME)/.local/bin

# Discover C source files and define targets
SOURCES = $(wildcard $(SRCDIR)/*.c)
TARGETS = $(patsubst $(SRCDIR)/%.c,$(BUILDDIR)/%,$(SOURCES))

# Default target
all: $(BUILDDIR) $(TARGETS)

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

$(BUILDDIR)/%: $(SRCDIR)/%.c
	$(CC) $(CFLAGS) -o $@ $<

# Test target
test: all
	@echo "Running C tests..."
	@if [ -f tests/c/test_mail_extract.sh ]; then chmod +x tests/c/test_mail_extract.sh && tests/c/test_mail_extract.sh; fi
	@echo "Running Python tests..."
	@# TODO: Add commands to run Python tests from tests/python/
	@echo "Running Elisp tests..."
	@# TODO: Add commands to run Elisp tests from tests/elisp/
	@echo "Tests finished."

# Install target
install: all
	mkdir -p $(INSTALLDIR)
ifneq ($(TARGETS),)
	cp $(TARGETS) $(INSTALLDIR)/
endif

# Clean target
clean:
	rm -rf $(BUILDDIR)
	find . -name "*.elc" -delete
