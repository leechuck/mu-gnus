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

## Configuration

The behavior of the C programs and Python scripts is controlled by a configuration file named `config.ini`.

To get started, copy the example configuration file:

```bash
cp config.ini.example config.ini
```

Then, edit `config.ini` to suit your needs. The application searches for this file in the following locations, in order:
1.  The current working directory (`./config.ini`)
2.  `~/.config/mail-assistant/config.ini`

The `config.ini` file contains settings for database paths, LLM providers, classification categories, and more. The `config.ini.example` file is commented to explain each setting.

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

It relies on `llm_client.py` for LLM communication.

#### llm_client.py

A unified client for interacting with LLMs. It can be used from the command line for testing.

```bash
echo "What is the capital of France?" | python3 src/python/llm_client.py
```

It supports multiple backends, configured via the `[llm]` section in `config.ini`. See `config.ini.example` for all options.

##### `cmd`

Executes a local command. The prompt is passed via stdin, and the command's stdout is used as the response.

- **type**: `cmd`
- **command**: The shell command to execute.

Example:
```ini
[llm]
type = cmd
command = echo '{"category": "test", "urgency": "low", "sender_type": "test"}'
```

##### `openai`

Connects to the OpenAI API or any OpenAI-compatible endpoint.

- **type**: `openai`
- **api_key**: Your OpenAI API key.
- **model**: The model to use (e.g., `gpt-4o`, `gpt-3.5-turbo`).
- **api_url**: (Optional) The API endpoint. Defaults to the official OpenAI URL.

Example:
```ini
[llm]
type = openai
api_key = sk-your-key-here
model = gpt-4o
```

##### `ollama`

Connects to a local [Ollama](https://ollama.com/) instance. You must have Ollama installed and running.

1.  Install Ollama.
2.  Pull a model: `ollama pull llama3`
3.  Ensure the Ollama server is running.

- **type**: `ollama`
- **model**: The model you have pulled (e.g., `llama3`, `mistral`).
- **api_url**: (Optional) The URL of your Ollama server. Defaults to `http://localhost:11434`.

Example:
```ini
[llm]
type = ollama
model = llama3
```

##### `openrouter`

Connects to the [OpenRouter](https://openrouter.ai/) API, which provides access to a wide variety of models.

- **type**: `openrouter`
- **api_key**: Your OpenRouter API key.
- **model**: The model identifier from OpenRouter (e.g., `google/gemini-pro`, `openai/gpt-4o`).

Example:
```ini
[llm]
type = openrouter
api_key = sk-or-your-key-here
model = google/gemini-pro
```

##### `gpt4all`

Connects to a local [GPT4All](https://gpt4all.io/) instance. You must have GPT4All installed and the API server enabled.

1.  Install GPT4All.
2.  Download a model within the GPT4All application.
3.  Enable the API server in GPT4All's settings.

- **type**: `gpt4all`
- **model**: The model file name (e.g., `ggml-gpt4all-j-v1.3-groovy.bin`). The client will use the first available model if this is left blank.
- **api_url**: (Optional) The URL of your GPT4All server. Defaults to `http://localhost:4891`.

Example:
```ini
[llm]
type = gpt4all
model = ggml-gpt4all-j-v1.3-groovy.bin
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
