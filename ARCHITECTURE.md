# System Architecture

This document provides a detailed overview of the components and data flow within the `mu-gnus` email assistant toolkit.

## Overview

The system is designed as a modular pipeline to process incoming emails. It classifies them, stores metadata in a database, and provides tools to interact with this data, primarily from within Emacs/Gnus.

The core workflow is as follows:
1.  An email is received and piped to the `mail-process` program.
2.  `mail-process` orchestrates the processing:
    a. It calls `mail-classify.py` to determine the email's category (e.g., "important", "newsletter").
    b. It extracts key headers like `Message-ID`, `From`, and `Subject`.
    c. It stores the email metadata and classification in a SQLite database using the `mail-db` command-line tool.
3.  The `mu-gnus.el` Emacs Lisp module provides functions to query the database (via `mail-db`) and display information within the Gnus email client, such as listing emails that need a reply.

## Core Components

### C Programs (Low-level Processing)

These are small, efficient, single-purpose utilities written in C. They form the backbone of the email processing pipeline.

*   **`mail-extract`**: A utility that parses a raw email from standard input. It can extract specific headers (case-insensitively), the email body, or all headers formatted as JSON. It correctly handles multi-line (folded) headers.

*   **`mail-db`**: A command-line interface to a SQLite database (`~/.mail.db` by default) that stores email metadata. It supports the following operations:
    *   `init`: Creates and initializes the database schema.
    *   `add`: Adds a new email record.
    *   `query`: Queries the database with various filters (e.g., `--needs-reply`, `--from`, `--classification`). Returns results in JSON format.
    *   `update`: Modifies an existing email record (e.g., to mark it as replied).

*   **`mail-process`**: The main orchestrator program. It reads a full email from stdin, then:
    1.  Checks if the email has an `X-Label` header. If so, it uses that for classification.
    2.  Otherwise, it invokes `src/python/mail-classify.py` to get a classification.
    3.  It extracts the `Message-ID`, `From`, and `Subject` headers.
    4.  It calls `mail-db add` to store this information in the database.
    5.  It exits with a specific code based on the classification, which can be used by mail delivery agents like `procmail`.

### Python Scripts (Intelligence and Formatting)

These scripts handle more complex logic, such as interacting with LLMs and converting email formats.

*   **`mail-classify.py`**: Classifies an email based on its content. It generates a prompt from the email's headers and body and sends it to a Large Language Model (LLM) for classification.
    *   It uses the `llm_client.py` for all LLM interactions.
    *   It maintains backward compatibility with older, direct-call methods if the unified client is unavailable.
    *   The script is designed to be called by `mail-process`.

*   **`llm_client.py`**: A unified client for interacting with various LLM backends.
    *   **Supported Backends**: Ollama, GPT4All, OpenAI, and any OpenAI-compatible API (like OpenRouter).
    *   **Configuration**: It is configured via `~/.mail-assistant/llm.conf`, where the user can specify the backend, model, API keys, and other parameters.
    *   **Caching**: It includes a file-based caching mechanism to avoid redundant API calls for the same prompt, saving time and cost.
    *   **CLI**: Provides a command-line interface for direct testing and interaction.

*   **`mail-to-org.py`**: A utility to convert an email into an Emacs Org mode entry. It can use a default or custom template to format the output.

### Emacs Lisp (User Interface)

*   **`mu-gnus.el`**: Provides the integration layer for the Gnus email client in Emacs.
    *   It defines functions that call the `mail-db` command-line tool to query the email database.
    *   `mu-gnus-show-needs-reply`: Fetches and displays a list of emails marked as needing a reply.
    *   `mu-gnus-mark-replied`: Marks the current email in Gnus as having been replied to by updating its entry in the database.
    *   `mu-gnus-add-to-db`: Manually adds the current email to the database.

## Data Flow

A typical data flow for a new email looks like this:

1.  **Mail Delivery Agent (MDA)** (e.g., `procmail` or a Sieve script) receives an email.
2.  The MDA pipes the raw email content to `bin/mail-process`.
3.  `mail-process` executes `src/python/mail-classify.py`.
4.  `mail-classify.py` reads the email from its stdin, creates a prompt, and uses `llm_client.py` to get a classification from a configured LLM.
5.  `llm_client.py` may make a network request to an LLM API (e.g., Ollama, OpenAI).
6.  `mail-classify.py` prints the classification (e.g., "important") to stdout.
7.  `mail-process` reads the classification. It also extracts headers from the email.
8.  `mail-process` executes `bin/mail-db add ...` with the extracted metadata and classification.
9.  `mail-db` writes the new record to the `~/.mail.db` SQLite file.
10. `mail-process` exits with a code corresponding to the classification. The MDA can use this exit code for further actions, like moving the email to a specific folder.

## Configuration

*   **LLM Client**: The LLM backend is configured in `~/.mail-assistant/llm.conf`. If this file doesn't exist, the client will not function.
*   **Database Path**: The path to the SQLite database can be set with the `MAIL_DB_PATH` environment variable. It defaults to `~/.mail.db`.
