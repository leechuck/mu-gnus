#!/bin/bash

# Test script for mail-process.c
# Run from the repository root directory

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test database and files
TEST_DB="/tmp/test_mail_process.db"
MAIL_PROCESS="./bin/mail-process"
MAIL_DB="./bin/mail-db"
TEST_OUTPUT="/tmp/test_mail_process_output.txt"
TEST_EMAIL="/tmp/test_email.txt"

# Clean up function
cleanup() {
    rm -f "$TEST_DB" "$TEST_OUTPUT" "$TEST_EMAIL"
    rm -f /tmp/test_email_*.txt
}

# Set up trap to clean up on exit
trap cleanup EXIT

# Check if binaries exist
if [ ! -f "$MAIL_PROCESS" ]; then
    echo "Error: mail-process binary not found at $MAIL_PROCESS"
    echo "Please run 'make' first to build the binary"
    exit 1
fi

if [ ! -f "$MAIL_DB" ]; then
    echo "Error: mail-db binary not found at $MAIL_DB"
    echo "Please run 'make' first to build the binary"
    exit 1
fi

echo "Running mail-process tests..."
echo

# Initialize test database
$MAIL_DB init "$TEST_DB" > /dev/null 2>&1

# Test 1: Process email with automated classification
echo -n "Test 1: Process email with automated classification... "
cat > "$TEST_EMAIL" << 'EOF'
From: noreply@system.example.com
To: user@example.com
Subject: System Notification
Message-ID: <auto123@example.com>
Date: Mon, 1 Jan 2024 10:00:00 +0000
Content-Type: text/plain

This is an automated system notification.
EOF

MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo automated" \
    $MAIL_PROCESS < "$TEST_EMAIL" > "$TEST_OUTPUT" 2>/dev/null
EXIT_CODE=$?

if [ $EXIT_CODE -eq 3 ] && grep -q "^X-Label: automated" "$TEST_OUTPUT"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Exit code: $EXIT_CODE (expected 3)"
    echo "  Output:"
    cat "$TEST_OUTPUT"
    exit 1
fi

# Test 2: Process email with important classification
echo -n "Test 2: Process email with important classification... "
cat > "$TEST_EMAIL" << 'EOF'
From: boss@company.com
To: employee@company.com
Subject: Urgent: Project Deadline
Message-ID: <important456@company.com>
Date: Tue, 2 Jan 2024 11:00:00 +0000

This is an important message about the project deadline.
Please respond immediately.
EOF

MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo important" \
    $MAIL_PROCESS < "$TEST_EMAIL" > "$TEST_OUTPUT" 2>/dev/null
EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ] && grep -q "^X-Label: important" "$TEST_OUTPUT"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Exit code: $EXIT_CODE (expected 0)"
    exit 1
fi

# Test 3: Process email with newsletter classification
echo -n "Test 3: Process email with newsletter classification... "
cat > "$TEST_EMAIL" << 'EOF'
From: newsletter@news.com
To: subscriber@example.com
Subject: Weekly Newsletter - January Edition
Message-ID: <news789@news.com>
Date: Wed, 3 Jan 2024 09:00:00 +0000

Welcome to our weekly newsletter!
Here are this week's top stories...
EOF

MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo newsletter" \
    $MAIL_PROCESS < "$TEST_EMAIL" > "$TEST_OUTPUT" 2>/dev/null
EXIT_CODE=$?

if [ $EXIT_CODE -eq 1 ] && grep -q "^X-Label: newsletter" "$TEST_OUTPUT"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Exit code: $EXIT_CODE (expected 1)"
    exit 1
fi

# Test 4: Process email with social classification
echo -n "Test 4: Process email with social classification... "
cat > "$TEST_EMAIL" << 'EOF'
From: notifications@social.com
To: user@example.com
Subject: You have a new follower!
Message-ID: <social321@social.com>
Date: Thu, 4 Jan 2024 14:00:00 +0000

Someone just followed you on Social Network.
Check out their profile!
EOF

MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo social" \
    $MAIL_PROCESS < "$TEST_EMAIL" > "$TEST_OUTPUT" 2>/dev/null
EXIT_CODE=$?

if [ $EXIT_CODE -eq 2 ] && grep -q "^X-Label: social" "$TEST_OUTPUT"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Exit code: $EXIT_CODE (expected 2)"
    exit 1
fi

# Test 5: Verify emails are stored in database
echo -n "Test 5: Verify emails are stored in database... "
DB_OUTPUT=$($MAIL_DB query --db "$TEST_DB" 2>/dev/null)

if echo "$DB_OUTPUT" | grep -q "auto123@example.com" && \
   echo "$DB_OUTPUT" | grep -q "important456@company.com" && \
   echo "$DB_OUTPUT" | grep -q "news789@news.com" && \
   echo "$DB_OUTPUT" | grep -q "social321@social.com"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Database output:"
    echo "$DB_OUTPUT"
    exit 1
fi

# Test 6: Verify important email has needs_reply flag
echo -n "Test 6: Verify important email has needs_reply flag... "
DB_OUTPUT=$($MAIL_DB query --needs-reply --db "$TEST_DB" 2>/dev/null)

if echo "$DB_OUTPUT" | grep -q "important456@company.com"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Expected important email to have needs_reply flag"
    exit 1
fi

# Test 7: Process email with existing X-Label (should not duplicate)
echo -n "Test 7: Process email with existing X-Label... "
cat > "$TEST_EMAIL" << 'EOF'
From: sender@example.com
To: recipient@example.com
Subject: Already Labeled
Message-ID: <labeled999@example.com>
X-Label: existing-label
Date: Fri, 5 Jan 2024 15:00:00 +0000

This email already has an X-Label header.
EOF

MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo newsletter" \
    $MAIL_PROCESS < "$TEST_EMAIL" > "$TEST_OUTPUT" 2>/dev/null

# Count X-Label headers
LABEL_COUNT=$(grep -c "^X-Label:" "$TEST_OUTPUT")

if [ $LABEL_COUNT -eq 1 ]; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Found $LABEL_COUNT X-Label headers (expected 1)"
    exit 1
fi

# Test 8: Process multipart email
echo -n "Test 8: Process multipart email... "
cat > "$TEST_EMAIL" << 'EOF'
From: multipart@example.com
To: recipient@example.com
Subject: Multipart Message
Message-ID: <multi111@example.com>
Date: Sat, 6 Jan 2024 16:00:00 +0000
MIME-Version: 1.0
Content-Type: multipart/alternative; boundary="boundary123"

--boundary123
Content-Type: text/plain

Plain text version of the email.
--boundary123
Content-Type: text/html

<html><body>HTML version</body></html>
--boundary123--
EOF

MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo automated" \
    $MAIL_PROCESS < "$TEST_EMAIL" > "$TEST_OUTPUT" 2>/dev/null
EXIT_CODE=$?

if [ $EXIT_CODE -eq 3 ] && grep -q "^X-Label: automated" "$TEST_OUTPUT" && \
   grep -q "multipart/alternative" "$TEST_OUTPUT"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Failed to process multipart email correctly"
    exit 1
fi

# Test 9: Process email with special characters in subject
echo -n "Test 9: Process email with special characters... "
cat > "$TEST_EMAIL" << 'EOF'
From: special@example.com
To: recipient@example.com
Subject: Special "Quote" & 'Apostrophe' Test
Message-ID: <special222@example.com>
Date: Sun, 7 Jan 2024 17:00:00 +0000

Testing special characters in headers.
EOF

MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo automated" \
    $MAIL_PROCESS < "$TEST_EMAIL" > "$TEST_OUTPUT" 2>/dev/null

# Check if email was processed without errors
if [ $? -eq 3 ] && grep -q "^X-Label: automated" "$TEST_OUTPUT"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Failed to handle special characters"
    exit 1
fi

# Test 10: Process email without Message-ID
echo -n "Test 10: Process email without Message-ID... "
cat > "$TEST_EMAIL" << 'EOF'
From: nomsgid@example.com
To: recipient@example.com
Subject: No Message ID
Date: Mon, 8 Jan 2024 18:00:00 +0000

This email has no Message-ID header.
EOF

MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo automated" \
    $MAIL_PROCESS < "$TEST_EMAIL" > "$TEST_OUTPUT" 2>/dev/null
EXIT_CODE=$?

# Should still process and add X-Label, but won't be in database
if [ $EXIT_CODE -eq 3 ] && grep -q "^X-Label: automated" "$TEST_OUTPUT"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Failed to process email without Message-ID"
    exit 1
fi

# Test 11: Large email processing
echo -n "Test 11: Process large email... "
# Create a large email (500KB)
cat > "$TEST_EMAIL" << 'EOF'
From: large@example.com
To: recipient@example.com
Subject: Large Email Test
Message-ID: <large333@example.com>
Date: Tue, 9 Jan 2024 19:00:00 +0000

EOF

# Add 500KB of content
for i in {1..5000}; do
    echo "This is line $i of a large email. It contains some text to make the email bigger." >> "$TEST_EMAIL"
done

MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo newsletter" \
    timeout 5 $MAIL_PROCESS < "$TEST_EMAIL" > "$TEST_OUTPUT" 2>/dev/null
EXIT_CODE=$?

if [ $EXIT_CODE -eq 1 ] && grep -q "^X-Label: newsletter" "$TEST_OUTPUT"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Failed to process large email"
    exit 1
fi

# Test 12: Verify body preservation
echo -n "Test 12: Verify body preservation... "
cat > "$TEST_EMAIL" << 'EOF'
From: preserve@example.com
To: recipient@example.com
Subject: Body Preservation Test
Message-ID: <preserve444@example.com>
Date: Wed, 10 Jan 2024 20:00:00 +0000

Line 1 of the body.
Line 2 with special chars: <>&"'
Line 3 with unicode: café résumé

Last line of the body.
EOF

MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo automated" \
    $MAIL_PROCESS < "$TEST_EMAIL" > "$TEST_OUTPUT" 2>/dev/null

# Check if body is preserved
if grep -q "Line 1 of the body" "$TEST_OUTPUT" && \
   grep -q "Line 2 with special chars: <>&\"'" "$TEST_OUTPUT" && \
   grep -q "Line 3 with unicode: café résumé" "$TEST_OUTPUT" && \
   grep -q "Last line of the body" "$TEST_OUTPUT"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Body content was not preserved correctly"
    exit 1
fi

# Test 13: Classification with failed classifier (fallback to automated)
echo -n "Test 13: Handle classifier failure... "
cat > "$TEST_EMAIL" << 'EOF'
From: fail@example.com
To: recipient@example.com
Subject: Classifier Failure Test
Message-ID: <fail555@example.com>
Date: Thu, 11 Jan 2024 21:00:00 +0000

This tests fallback when classifier fails.
EOF

# Use a command that will fail
MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="false" \
    $MAIL_PROCESS < "$TEST_EMAIL" > "$TEST_OUTPUT" 2>/dev/null
EXIT_CODE=$?

# Should fallback to automated classification
if [ $EXIT_CODE -eq 3 ] && grep -q "^X-Label: automated" "$TEST_OUTPUT"; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Did not fallback to automated classification"
    exit 1
fi

# Test 14: Concurrent processing (stress test)
echo -n "Test 14: Concurrent processing... "
for i in {1..5}; do
    cat > "/tmp/test_email_$i.txt" << EOF
From: concurrent$i@example.com
To: recipient@example.com
Subject: Concurrent Test $i
Message-ID: <concurrent$i@example.com>
Date: Fri, 12 Jan 2024 22:0$i:00 +0000

Concurrent email $i
EOF
done

# Process multiple emails concurrently
for i in {1..5}; do
    (MAIL_DB_PATH="$TEST_DB" MAIL_LLM_TYPE=cmd MAIL_LLM_CMD="echo automated" \
        $MAIL_PROCESS < "/tmp/test_email_$i.txt" > "/tmp/test_output_$i.txt" 2>/dev/null) &
done

# Wait for all background jobs
wait

# Check all outputs
SUCCESS=true
for i in {1..5}; do
    if ! grep -q "^X-Label: automated" "/tmp/test_output_$i.txt"; then
        SUCCESS=false
        break
    fi
done

if $SUCCESS; then
    echo -e "${GREEN}PASS${NC}"
else
    echo -e "${RED}FAIL${NC}"
    echo "  Concurrent processing failed"
    exit 1
fi

# Clean up concurrent test files
rm -f /tmp/test_output_*.txt

echo
echo -e "${GREEN}All mail-process tests passed!${NC}"
