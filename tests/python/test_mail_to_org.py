#!/usr/bin/env python3

import unittest
import sys
import os
import importlib.util
from datetime import datetime
from unittest.mock import patch, MagicMock
import tempfile
import email
from email.message import EmailMessage

# Import the module under test
spec = importlib.util.spec_from_file_location("mail_to_org", 
                                               os.path.join(os.path.dirname(__file__), 
                                                          '../../src/python/mail-to-org.py'))
mail_to_org = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mail_to_org)

class TestEmailToOrg(unittest.TestCase):
    
    def setUp(self):
        self.converter = mail_to_org.EmailToOrg()
        
        # Sample plain text email
        self.sample_plain_email = """From: sender@example.com
To: recipient@example.com
Subject: Test Email Subject
Message-ID: <12345@example.com>
Date: Mon, 1 Jan 2024 10:00:00 +0000
Content-Type: text/plain; charset="utf-8"

This is a test email body.
It has multiple lines.

And even paragraphs.
"""
        
        # Sample multipart email
        self.sample_multipart_email = """From: alice@example.org
To: bob@example.org
Subject: Multipart Test Message
Message-ID: <abc123@example.org>
Date: Tue, 2 Jan 2024 14:30:00 +0000
MIME-Version: 1.0
Content-Type: multipart/alternative; boundary="boundary123"

--boundary123
Content-Type: text/plain; charset="utf-8"

This is the plain text version.
With multiple lines.

--boundary123
Content-Type: text/html; charset="utf-8"

<html>
<body>
<p>This is the <b>HTML</b> version.</p>
<p>With multiple paragraphs.</p>
</body>
</html>

--boundary123--
"""
        
        # Sample HTML-only email
        self.sample_html_email = """From: webmaster@example.net
To: user@example.net
Subject: HTML Only Email
Message-ID: <html567@example.net>
Date: Wed, 3 Jan 2024 09:15:00 +0000
Content-Type: text/html; charset="utf-8"

<html>
<head><title>Test</title></head>
<body>
<h1>Welcome</h1>
<p>This is an <strong>HTML only</strong> email.</p>
<ul>
<li>Item 1</li>
<li>Item 2</li>
</ul>
</body>
</html>
"""
        
        # Sample email with special characters
        self.sample_special_chars_email = """From: test@example.com
To: recipient@example.com
Subject: Email with * Special Characters
Message-ID: <special789@example.com>
Date: Thu, 4 Jan 2024 11:45:00 +0000
Content-Type: text/plain; charset="utf-8"

* This line starts with an asterisk
** This has two asterisks
Regular text here.
* Another asterisk line
"""

    def test_read_email_from_file(self):
        """Test reading email from a file."""
        with tempfile.NamedTemporaryFile(mode='wb', delete=False) as f:
            f.write(self.sample_plain_email.encode('utf-8'))
            temp_file = f.name
        
        try:
            msg = self.converter.read_email(temp_file)
            self.assertEqual(msg['Subject'], 'Test Email Subject')
            self.assertEqual(msg['From'], 'sender@example.com')
        finally:
            os.unlink(temp_file)
    
    def test_extract_body_plain_text(self):
        """Test extracting body from plain text email."""
        msg = email.message_from_string(self.sample_plain_email)
        body = self.converter.extract_body(msg)
        self.assertIn("This is a test email body", body)
        self.assertIn("It has multiple lines", body)
        self.assertIn("And even paragraphs", body)
    
    def test_extract_body_multipart(self):
        """Test extracting body from multipart email."""
        msg = email.message_from_string(self.sample_multipart_email)
        body = self.converter.extract_body(msg)
        # Should prefer plain text version
        self.assertIn("This is the plain text version", body)
        self.assertIn("With multiple lines", body)
    
    def test_extract_body_html_only(self):
        """Test extracting body from HTML-only email."""
        msg = email.message_from_string(self.sample_html_email)
        body = self.converter.extract_body(msg)
        # Should convert HTML to text
        self.assertIn("Welcome", body)
        self.assertIn("HTML only", body)
        self.assertIn("Item 1", body)
        self.assertIn("Item 2", body)
        # Should not contain HTML tags
        self.assertNotIn("<strong>", body)
        self.assertNotIn("<h1>", body)
    
    def test_html_to_text(self):
        """Test HTML to text conversion."""
        html = """<html>
        <head><style>body { color: red; }</style></head>
        <body>
        <h1>Title</h1>
        <p>Paragraph with <b>bold</b> and <i>italic</i>.</p>
        <script>alert('test');</script>
        <div>Another &lt;div&gt; with &amp; entities.</div>
        </body>
        </html>"""
        
        text = self.converter.html_to_text(html)
        self.assertIn("Title", text)
        self.assertIn("Paragraph with bold and italic", text)
        self.assertIn("Another <div> with & entities", text)
        self.assertNotIn("style", text)
        self.assertNotIn("script", text)
        self.assertNotIn("alert", text)
    
    def test_format_date(self):
        """Test date formatting."""
        # Valid RFC2822 date
        date_str = "Mon, 1 Jan 2024 10:00:00 +0000"
        formatted = self.converter.format_date(date_str)
        self.assertIn("2024-01-01", formatted)
        self.assertIn("Mon", formatted)
        
        # Invalid date should return current date format
        invalid_date = "Not a date"
        formatted = self.converter.format_date(invalid_date)
        self.assertRegex(formatted, r'\d{4}-\d{2}-\d{2} \w{3} \d{2}:\d{2}')
        
        # Empty date
        formatted = self.converter.format_date("")
        self.assertRegex(formatted, r'\d{4}-\d{2}-\d{2} \w{3} \d{2}:\d{2}')
    
    def test_escape_org_text(self):
        """Test escaping special characters for org-mode."""
        text = """* This starts with asterisk
** This has two
Regular line
  * Indented asterisk
Another regular line"""
        
        escaped = self.converter.escape_org_text(text)
        lines = escaped.split('\n')
        self.assertEqual(lines[0], ",* This starts with asterisk")
        self.assertEqual(lines[1], ",** This has two")
        self.assertEqual(lines[2], "Regular line")
        self.assertEqual(lines[3], ",  * Indented asterisk")
        self.assertEqual(lines[4], "Another regular line")
    
    def test_convert_to_org_default_format(self):
        """Test converting email to default org format."""
        msg = email.message_from_string(self.sample_plain_email)
        org_content = self.converter.convert_to_org(msg)
        
        # Check TODO heading
        self.assertIn("* TODO Test Email Subject", org_content)
        
        # Check properties
        self.assertIn(":PROPERTIES:", org_content)
        self.assertIn(":MESSAGE_ID: <12345@example.com>", org_content)
        self.assertIn(":FROM: sender@example.com", org_content)
        self.assertIn(":DATE: 2024-01-01 Mon 10:00", org_content)
        self.assertIn(":END:", org_content)
        
        # Check body in quote block
        self.assertIn("#+BEGIN_QUOTE", org_content)
        self.assertIn("This is a test email body", org_content)
        self.assertIn("#+END_QUOTE", org_content)
    
    def test_convert_to_org_with_scheduled(self):
        """Test adding SCHEDULED property."""
        converter = mail_to_org.EmailToOrg(scheduled=True)
        msg = email.message_from_string(self.sample_plain_email)
        
        # Mock datetime.now() to return a fixed date
        with patch.object(mail_to_org, 'datetime') as mock_datetime:
            mock_datetime.now.return_value = datetime(2024, 1, 15, 12, 0, 0)
            mock_datetime.strftime = datetime.strftime
            
            org_content = converter.convert_to_org(msg)
            self.assertIn("SCHEDULED: <2024-01-15 Mon>", org_content)
    
    def test_convert_with_special_characters(self):
        """Test converting email with special org-mode characters."""
        msg = email.message_from_string(self.sample_special_chars_email)
        org_content = self.converter.convert_to_org(msg)
        
        # Check that asterisks in body are escaped
        self.assertIn("#+BEGIN_QUOTE", org_content)
        # The body should have escaped asterisks
        quote_start = org_content.index("#+BEGIN_QUOTE")
        quote_end = org_content.index("#+END_QUOTE")
        quote_content = org_content[quote_start:quote_end]
        self.assertIn(",* This line starts with an asterisk", quote_content)
        self.assertIn(",** This has two asterisks", quote_content)
    
    def test_custom_template(self):
        """Test using a custom template."""
        template_content = """** {{SUBJECT}}
{{SCHEDULED}}
:PROPERTIES:
:EMAIL_FROM: {{FROM}}
:EMAIL_ID: {{MESSAGE_ID}}
:EMAIL_DATE: {{DATE}}
:END:

{{BODY}}
"""
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.org') as f:
            f.write(template_content)
            template_file = f.name
        
        try:
            converter = mail_to_org.EmailToOrg(template_file=template_file, scheduled=True)
            msg = email.message_from_string(self.sample_plain_email)
            
            # Mock datetime.now() to return a fixed date
            with patch.object(mail_to_org, 'datetime') as mock_datetime:
                mock_datetime.now.return_value = datetime(2024, 1, 15, 12, 0, 0)
                mock_datetime.strftime = datetime.strftime
                
                org_content = converter.convert_to_org(msg)
                
                self.assertIn("** Test Email Subject", org_content)
                self.assertIn("SCHEDULED: <2024-01-15 Mon>", org_content)
                self.assertIn(":EMAIL_FROM: sender@example.com", org_content)
                self.assertIn(":EMAIL_ID: <12345@example.com>", org_content)
                self.assertIn("This is a test email body", org_content)
        finally:
            os.unlink(template_file)
    
    def test_multipart_with_attachment(self):
        """Test handling multipart email with attachment."""
        email_with_attachment = """From: sender@example.com
To: recipient@example.com
Subject: Email with Attachment
Message-ID: <attach123@example.com>
Date: Fri, 5 Jan 2024 15:00:00 +0000
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary="boundary456"

--boundary456
Content-Type: text/plain; charset="utf-8"

This email has an attachment.

--boundary456
Content-Type: application/pdf; name="document.pdf"
Content-Disposition: attachment; filename="document.pdf"
Content-Transfer-Encoding: base64

JVBERi0xLjQKJeLjz9MKNCAwIG9iago=

--boundary456--
"""
        
        msg = email.message_from_string(email_with_attachment)
        body = self.converter.extract_body(msg)
        
        # Should extract text but not attachment
        self.assertIn("This email has an attachment", body)
        self.assertNotIn("JVBERi0xLjQK", body)  # Should not include base64 content
    
    def test_empty_email(self):
        """Test handling email with minimal headers and no body."""
        empty_email = """From: sender@example.com
Subject: Empty

"""
        
        msg = email.message_from_string(empty_email)
        org_content = self.converter.convert_to_org(msg)
        
        self.assertIn("* TODO Empty", org_content)
        self.assertIn("#+BEGIN_QUOTE", org_content)
        self.assertIn("[Empty email body]", org_content)
        self.assertIn("#+END_QUOTE", org_content)
    
    def test_run_method(self):
        """Test the main run method."""
        with tempfile.NamedTemporaryFile(mode='wb', delete=False) as f:
            f.write(self.sample_plain_email.encode('utf-8'))
            temp_file = f.name
        
        try:
            # Capture stdout
            from io import StringIO
            import sys
            captured_output = StringIO()
            sys.stdout = captured_output
            
            result = self.converter.run(temp_file)
            
            sys.stdout = sys.__stdout__
            output = captured_output.getvalue()
            
            self.assertEqual(result, 0)
            self.assertIn("* TODO Test Email Subject", output)
            self.assertIn(":MESSAGE_ID: <12345@example.com>", output)
        finally:
            os.unlink(temp_file)
    
    def test_run_method_with_error(self):
        """Test run method with invalid input."""
        from io import StringIO
        # Try to read non-existent file and capture stderr
        with patch('sys.stderr', new_callable=StringIO) as mock_stderr:
            result = self.converter.run("/non/existent/file.txt")
            self.assertEqual(result, 1)
            # Verify that an error message was printed to stderr
            self.assertIn("No such file or directory", mock_stderr.getvalue())

if __name__ == '__main__':
    unittest.main()
