#!/bin/bash

# Test script for mail-db.c
# Run from the repository root directory

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Test database path
TEST_DB="/tmp/test_mail.db"
MAIL_DB="./bin/mail-db"

# Clean up function
cleanup() {
    rm -f "$TEST_DB"
}

# Set up trap to clean up on exit
trap cleanup EXIT

# Check if mail-db binary exists
if [ ! -f "$MAIL_DB" ]; then
    echo "Error: mail-db binary not found at $MAIL_DB"
    echo "Please run 'make' first to build the binary"
    exit 1
fi

echo "Running mail-db tests..."
echo

# Test 1: Initialize database
echo -n "Test 1: Initialize database... "
if $MAIL_DB init "$TEST_DB" > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 2: Add email without needs-reply
echo -n "Test 2: Add email (basic)... "
if $MAIL_DB add "msg001" --from "alice@example.com" --subject "Hello World" > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 3: Add email with needs-reply
echo -n "Test 3: Add email with needs-reply... "
if $MAIL_DB add "msg002" --from "bob@example.com" --subject "Important Question" --needs-reply --classification "work" > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 4: Add email with custom date
echo -n "Test 4: Add email with custom date... "
if $MAIL_DB add "msg003" --from "charlie@example.com" --subject "Old Message" --date 1609459200 > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 5: Query all emails
echo -n "Test 5: Query all emails... "
RESULT=$($MAIL_DB query 2>/dev/null)
if echo "$RESULT" | grep -q "msg001" && echo "$RESULT" | grep -q "msg002" && echo "$RESULT" | grep -q "msg003"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Output: $RESULT"
    exit 1
fi

# Test 6: Query emails needing reply
echo -n "Test 6: Query emails needing reply... "
RESULT=$($MAIL_DB query --needs-reply 2>/dev/null)
if echo "$RESULT" | grep -q "msg002" && ! echo "$RESULT" | grep -q "msg001"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Output: $RESULT"
    exit 1
fi

# Test 7: Query by from address
echo -n "Test 7: Query by from address... "
RESULT=$($MAIL_DB query --from "alice@example.com" 2>/dev/null)
if echo "$RESULT" | grep -q "msg001" && ! echo "$RESULT" | grep -q "msg002"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Output: $RESULT"
    exit 1
fi

# Test 8: Query by classification
echo -n "Test 8: Query by classification... "
RESULT=$($MAIL_DB query --classification "work" 2>/dev/null)
if echo "$RESULT" | grep -q "msg002" && ! echo "$RESULT" | grep -q "msg001"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Output: $RESULT"
    exit 1
fi

# Test 9: Update email as replied
echo -n "Test 9: Update email as replied... "
if $MAIL_DB update "msg002" --replied 1 > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 10: Query not-replied emails
echo -n "Test 10: Query not-replied emails... "
RESULT=$($MAIL_DB query --not-replied 2>/dev/null)
if ! echo "$RESULT" | grep -q "msg002" && echo "$RESULT" | grep -q "msg001"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Output: $RESULT"
    exit 1
fi

# Test 11: Update classification
echo -n "Test 11: Update classification... "
if $MAIL_DB update "msg001" --classification "personal" > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 12: Verify classification update
echo -n "Test 12: Verify classification update... "
RESULT=$($MAIL_DB query --classification "personal" 2>/dev/null)
if echo "$RESULT" | grep -q "msg001"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Output: $RESULT"
    exit 1
fi

# Test 13: Update needs_reply flag
echo -n "Test 13: Update needs_reply flag... "
if $MAIL_DB update "msg001" --needs-reply 1 > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 14: Add email with special characters in subject
echo -n "Test 14: Add email with special characters... "
if $MAIL_DB add "msg004" --from "test@example.com" --subject "Quote\" and \\ backslash" > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 15: Verify JSON escaping
echo -n "Test 15: Verify JSON escaping... "
RESULT=$($MAIL_DB query 2>/dev/null)
if echo "$RESULT" | grep -q 'Quote\\" and \\\\ backslash'; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Output: $RESULT"
    exit 1
fi

# Test 16: Update non-existent message
echo -n "Test 16: Update non-existent message (should fail)... "
if ! $MAIL_DB update "nonexistent" --replied 1 > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 17: Add duplicate message ID (should fail)
echo -n "Test 17: Add duplicate message ID (should fail)... "
if ! $MAIL_DB add "msg001" --from "duplicate@example.com" --subject "Duplicate" > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    exit 1
fi

# Test 18: Query with multiple filters
echo -n "Test 18: Query with multiple filters... "
$MAIL_DB add "msg005" --from "alice@example.com" --subject "Another message" --needs-reply > /dev/null 2>&1
RESULT=$($MAIL_DB query --needs-reply --from "alice@example.com" 2>/dev/null)
if echo "$RESULT" | grep -q "msg005" && echo "$RESULT" | grep -q "msg001"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Output: $RESULT"
    exit 1
fi

# Test 19: Verify JSON format
echo -n "Test 19: Verify JSON format... "
RESULT=$($MAIL_DB query 2>/dev/null)
if python3 -c "import json; import sys; json.loads(sys.stdin.read())" <<< "$RESULT" 2>/dev/null; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Invalid JSON output"
    exit 1
fi

# Test 20: Environment variable for database path
echo -n "Test 20: Environment variable for database path... "
export MAIL_DB_PATH="$TEST_DB"
RESULT=$($MAIL_DB query 2>/dev/null)
if echo "$RESULT" | grep -q "msg001"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "Output: $RESULT"
    exit 1
fi
unset MAIL_DB_PATH

echo
echo -e "${GREEN}All tests passed!${NC}"
