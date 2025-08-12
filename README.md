# mu-gnus

A collection of tools to assist with email management, particularly for use with Emacs, Gnus, and `mu`.

## Prerequisites

To build the C programs, you need a C compiler and `make`. On Debian 13 or other Debian-based systems, you can install the necessary packages with:

```bash
sudo apt-get update && sudo apt-get install build-essential
```

## Building

To compile the C programs, run:

```bash
make
```

The binaries will be placed in the `bin/` directory.

## Installation

To install the compiled programs into `$HOME/.local/bin/`, run:

```bash
make install
```

Ensure that `$HOME/.local/bin` is in your `PATH`.

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

## Running Tests

To run the test suite for all components, run:

```bash
make test
```

This will execute tests for:
*   C programs (`tests/c/`)
*   Python scripts (`tests/python/`)
*   Emacs Lisp code (`tests/elisp/`)
