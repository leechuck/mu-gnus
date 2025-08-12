#!/bin/bash

# Test runner for C programs.
# This script should be run from the project root.

MAIL_EXTRACT_BIN="./bin/mail-extract"
SAMPLE_EMAIL="./tests/c/sample_email.txt"
FAILURES=0

echo "--- Running tests for mail-extract ---"

# Helper for reporting test results
report_result() {
    if [ $? -eq 0 ]; then
        echo "PASS: $1"
    else
        echo "FAIL: $1"
        FAILURES=$((FAILURES + 1))
    fi
}

# Test 1: Extract 'From' header
DESCRIPTION="Extract 'From' header"
(
    OUTPUT=$(cat "$SAMPLE_EMAIL" | "$MAIL_EXTRACT_BIN" --header From)
    [ "$OUTPUT" = "sender@example.com" ]
)
report_result "$DESCRIPTION"

# Test 2: Extract 'subject' header (case-insensitive)
DESCRIPTION="Extract 'subject' header (case-insensitive)"
(
    OUTPUT=$(cat "$SAMPLE_EMAIL" | "$MAIL_EXTRACT_BIN" --header subject)
    [ "$OUTPUT" = "Test Email" ]
)
report_result "$DESCRIPTION"

# Test 3: Extract multi-line 'X-Custom-Header'
DESCRIPTION="Extract multi-line 'X-Custom-Header'"
(
    OUTPUT=$(cat "$SAMPLE_EMAIL" | "$MAIL_EXTRACT_BIN" --header X-Custom-Header)
    [ "$OUTPUT" = "some value that spans multiple lines" ]
)
report_result "$DESCRIPTION"

# Test 4: Extract email body
DESCRIPTION="Extract email body"
(
    OUTPUT=$(cat "$SAMPLE_EMAIL" | "$MAIL_EXTRACT_BIN" --body)
    EXPECTED_BODY="This is the body of the email.
It has multiple lines.
"
    [ "$OUTPUT" = "$EXPECTED_BODY" ]
)
report_result "$DESCRIPTION"

# Test 5: Extract headers as JSON
DESCRIPTION="Extract headers as JSON"
(
    OUTPUT=$(cat "$SAMPLE_EMAIL" | "$MAIL_EXTRACT_BIN" --json)
    EXPECTED_JSON='{
  "From": "sender@example.com",
  "To": "recipient@example.com",
  "Subject": "Test Email",
  "Date": "Tue, 12 Aug 2025 10:00:00 +0000",
  "Content-Type": "text/plain; charset=\"UTF-8\"",
  "X-Custom-Header": "some value that spans multiple lines"
}'
    NORMALIZED_OUTPUT=$(echo "$OUTPUT" | tr -d ' \n')
    NORMALIZED_EXPECTED=$(echo "$EXPECTED_JSON" | tr -d ' \n')
    [ "$NORMALIZED_OUTPUT" = "$NORMALIZED_EXPECTED" ]
)
report_result "$DESCRIPTION"

# Test 6: Error on no arguments
DESCRIPTION="Error on no arguments"
(
    ! "$MAIL_EXTRACT_BIN" < "$SAMPLE_EMAIL" >/dev/null 2>&1
)
report_result "$DESCRIPTION"

# Test 7: Error on --header with no name
DESCRIPTION="Error on --header with no name"
(
    ! "$MAIL_EXTRACT_BIN" --header < "$SAMPLE_EMAIL" >/dev/null 2>&1
)
report_result "$DESCRIPTION"


if [ $FAILURES -ne 0 ]; then
    echo "--- $FAILURES C test(s) failed. ---"
    exit 1
fi

echo "--- All C tests passed. ---"
