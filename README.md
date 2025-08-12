# mu-gnus

A collection of tools to assist with email management, particularly for use with Emacs, Gnus, and `mu`.

For a detailed explanation of how the components work together, see [ARCHITECTURE.md](./ARCHITECTURE.md).

## Prerequisites

### C Tools

To build the C programs, you need a C compiler, `make`, and SQLite development libraries. On Debian-based systems, you can install them with:

```bash
sudo apt-get update && sudo apt-get install build-essential libsqlite3-dev
```

This will install:
- `gcc` - The GNU C compiler
- `make` - The GNU make utility
- `libsqlite3-dev` - Development files for SQLite, required by `mail-db`.

### Python Scripts

The Python scripts require Python 3 and some external libraries. You can install them using `pip`.

```bash
sudo apt-get install python3-pip
pip install requests
```

You will also need to create a configuration file for the LLM client at `~/.mail-assistant/llm.conf`.

## Building

The project uses a Makefile to manage compilation. To compile all C programs:

```bash
make
```

This will:
1. Create a `bin/` directory if it doesn't exist
2. Compile all C source files from `src/c/` 
3. Place the resulting binaries in `bin/`

To compile with debug symbols:

```bash
make CFLAGS="-Wall -Wextra -std=c99 -g"
```

To clean build artifacts:

```bash
make clean
```

This removes the `bin/` directory and any compiled Emacs Lisp files (`.elc`).

## Installation

To install the compiled programs into `$HOME/.local/bin/`, run:

```bash
make install
```

Ensure that `$HOME/.local/bin` is in your `PATH`. You can add it by adding this line to your `~/.bashrc` or `~/.zshrc`:

```bash
export PATH="$HOME/.local/bin:$PATH"
```

## Usage

See [ARCHITECTURE.md](./ARCHITECTURE.md) for a detailed description of how these components work together.

### C Programs

#### mail-extract

Extracts parts of an email from standard input.

*   **Extract a header:** `cat email.txt | mail-extract --header From`
*   **Extract the body:** `cat email.txt | mail-extract --body`
*   **Extract all headers as JSON:** `cat email.txt | mail-extract --json`

#### mail-db

A command-line interface to the email metadata database.

*   **Initialize the database:** `mail-db init ~/.mail.db`
*   **Add an email record:** `mail-db add <msg-id> --from "..." --subject "..."`
*   **Query for emails needing a reply:** `mail-db query --needs-reply`
*   **Update an email record:** `mail-db update <msg-id> --replied 1`

The database path can also be set via the `MAIL_DB_PATH` environment variable.

#### mail-process

The core pipeline script that reads an email, classifies it, and adds it to the database. It's typically called by a Mail Delivery Agent (e.g., Procmail).

```bash
cat email.txt | mail-process
```

It uses `mail-classify.py` for classification and `mail-db` for database operations.

### Python Scripts

#### mail-classify.py

Classifies an email using an LLM. This script is intended to be called by `mail-process`.

It relies on `llm_client.py` for LLM communication, which must be configured.

#### llm_client.py

A unified client for interacting with LLMs. It can be used from the command line for testing.

```bash
echo "What is the capital of France?" | python3 src/python/llm_client.py
```

It requires a configuration file at `~/.mail-assistant/llm.conf`. The file uses an INI format:

```ini
[llm]
backend = ollama
# For Ollama
ollama_url = http://localhost:11434/api/generate
ollama_model = llama3

# For OpenAI
# backend = openai
# openai_api_key = sk-xxxxxxxxxx
# openai_model = gpt-4
```

#### mail-to-org.py

Converts an email from stdin to an Org mode file.

```bash
cat email.txt | python3 src/python/mail-to-org.py > email.org
```

### Emacs Lisp

#### mu-gnus.el

Provides functions for Gnus to interact with the mail database. After loading it in your Emacs configuration, you can use functions like `mu-gnus-show-needs-reply` to see a list of emails that require a response.

## Running Tests

To run the test suite for all components, run:

```bash
make test
```

This will execute tests for:
*   C programs (`tests/c/`)
*   Python scripts (`tests/python/`)
*   Emacs Lisp code (`tests/elisp/`)

To run only the C tests:

```bash
./tests/c/run_tests.sh
```

The test suite verifies:
- Header extraction (including case-insensitive matching)
- Multi-line header handling
- Body extraction
- JSON output formatting
- Error handling for invalid arguments

## Project Structure

```
.
├── ARCHITECTURE.md       # System architecture overview
├── bin/                  # Compiled binaries (created by make)
├── doc/                  # Documentation
├── elisp/                # Emacs Lisp code
├── procmail/             # Procmail examples and configurations
├── src/
│   ├── c/                # C source files
│   └── python/           # Python scripts
├── tests/
│   ├── c/                # C program tests
│   ├── elisp/            # Emacs Lisp tests
│   └── python/           # Python script tests
├── Makefile              # Build configuration
└── README.md             # This file
```

## Contributing

When adding new C programs:
1. Place the source file in `src/c/`
2. The Makefile will automatically detect and compile it
3. Add corresponding tests in `tests/c/`
4. Update this README with usage information
