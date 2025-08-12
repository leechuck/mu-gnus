# mu-gnus

A collection of tools to assist with email management, particularly for use with Emacs, Gnus, and `mu`.

## Prerequisites

To build the C programs, you need a C compiler and `make`. On Debian 13 or other Debian-based systems, you can install the necessary packages with:

```bash
sudo apt-get update && sudo apt-get install build-essential
```

This will install:
- `gcc` - The GNU C compiler
- `make` - The GNU make utility
- Other essential build tools

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

### mail-extract

This program extracts parts of an email from standard input.

*   Extract a specific header:
    ```bash
    cat email.txt | mail-extract --header From
    ```
*   Extract the email body:
    ```bash
    cat email.txt | mail-extract --body
    ```
*   Extract all headers as JSON:
    ```bash
    cat email.txt | mail-extract --json
    ```

The program handles:
- Case-insensitive header matching
- Multi-line header values (RFC 2822 folding)
- Headers with special characters

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
