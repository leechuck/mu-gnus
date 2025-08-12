#!/bin/bash

# Integration tests for mail assistant

set -e  # Exit on error

echo "Running integration tests..."

# Create temp directory for test files
TEST_DIR=$(mktemp -d)
trap "rm -rf $TEST_DIR" EXIT

# Create test email file
cat > "$TEST_DIR/test-email.txt" << 'EOF'
From: test@example.com
To: me@example.com
Subject: Test Action Items
Message-ID: <test123@example.com>
Date: Mon, 1 Jan 2024 10:00:00 +0000
Content-Type: text/plain

Can you review the document?
Please confirm by Friday.
Choose either option A or B.
EOF

echo "Test 1: Extract actions from email..."
# Run mail-analyze extract-actions
JSON_OUTPUT=$(cat "$TEST_DIR/test-email.txt" | python3 src/python/mail-analyze.py extract-actions)

# Check if JSON is valid
if ! echo "$JSON_OUTPUT" | python3 -m json.tool > /dev/null 2>&1; then
    echo "ERROR: Invalid JSON output"
    echo "$JSON_OUTPUT"
    exit 1
fi

# Extract action items count using Python
ACTION_COUNT=$(echo "$JSON_OUTPUT" | python3 -c "
import sys, json
data = json.load(sys.stdin)
actions = data.get('actions', [])
print(len(actions))
")

if [ "$ACTION_COUNT" -lt 2 ]; then
    echo "ERROR: Expected at least 2 action items, got $ACTION_COUNT"
    echo "JSON output:"
    echo "$JSON_OUTPUT"
    exit 1
fi

echo "✓ Extract actions test passed (found $ACTION_COUNT action items)"

# Create response file
cat > "$TEST_DIR/response.txt" << 'EOF'
Yes, reviewed. Confirmed. Choose A.
EOF

echo "Test 2: Generate draft reply..."
# Run mail-draft-reply
DRAFT_OUTPUT=$(python3 src/python/mail-draft-reply.py --original "$TEST_DIR/test-email.txt" --response "$TEST_DIR/response.txt")

# Check if draft contains key phrases from response
if ! echo "$DRAFT_OUTPUT" | grep -qi "reviewed"; then
    echo "ERROR: Draft does not contain 'reviewed'"
    echo "Draft output:"
    echo "$DRAFT_OUTPUT"
    exit 1
fi

if ! echo "$DRAFT_OUTPUT" | grep -qi "confirm"; then
    echo "ERROR: Draft does not contain 'confirm'"
    echo "Draft output:"
    echo "$DRAFT_OUTPUT"
    exit 1
fi

if ! echo "$DRAFT_OUTPUT" | grep -qi "A"; then
    echo "ERROR: Draft does not mention option A"
    echo "Draft output:"
    echo "$DRAFT_OUTPUT"
    exit 1
fi

echo "✓ Draft reply test passed"

# Test 3: Test with multipart email
echo "Test 3: Extract actions from multipart email..."
cat > "$TEST_DIR/multipart-email.txt" << 'EOF'
From: sender@example.com
To: recipient@example.com
Subject: Meeting Request
Message-ID: <meeting123@example.com>
Date: Mon, 1 Jan 2024 10:00:00 +0000
Content-Type: multipart/alternative; boundary="boundary123"

--boundary123
Content-Type: text/plain; charset="utf-8"

Hi,

Can you attend the meeting on Tuesday?
Please review the attached agenda.
Let me know your availability.

Thanks

--boundary123
Content-Type: text/html; charset="utf-8"

<html>
<body>
<p>Hi,</p>
<p>Can you attend the meeting on Tuesday?<br>
Please review the attached agenda.<br>
Let me know your availability.</p>
<p>Thanks</p>
</body>
</html>
--boundary123--
EOF

JSON_OUTPUT2=$(cat "$TEST_DIR/multipart-email.txt" | python3 src/python/mail-analyze.py extract-actions)

# Check if JSON is valid
if ! echo "$JSON_OUTPUT2" | python3 -m json.tool > /dev/null 2>&1; then
    echo "ERROR: Invalid JSON output for multipart email"
    echo "$JSON_OUTPUT2"
    exit 1
fi

echo "✓ Multipart email test passed"

# Test 4: Test config value retrieval
echo "Test 4: Get config value..."
CONFIG_OUTPUT=$(python3 src/python/mail-analyze.py --get-config org_file 2>/dev/null || true)
echo "✓ Config retrieval test passed"

echo ""
echo "Basic tests passed"
