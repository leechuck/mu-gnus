#!/usr/bin/env python3
"""
Unit tests for mail-classify.py
"""

import unittest
import sys
import os
import tempfile
from unittest.mock import patch, MagicMock, mock_open
from io import StringIO
import json

# Add src directory to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../src/python'))

# Mock llm_client before importing mail_classify
mock_llm_client_module = MagicMock()
sys.modules['llm_client'] = mock_llm_client_module

import importlib.util
spec = importlib.util.spec_from_file_location("mail_classify", 
                                               os.path.join(os.path.dirname(__file__), 
                                                          '../../src/python/mail-classify.py'))
mail_classify = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mail_classify)


class TestEmailClassifier(unittest.TestCase):
    
    def setUp(self):
        # Mock the LLMClient
        self.mock_llm_client_instance = MagicMock()
        mock_llm_client_module.LLMClient.return_value = self.mock_llm_client_instance
        
        self.classifier = mail_classify.EmailClassifier()
        self.sample_email = """From: sender@example.com
To: recipient@example.com
Subject: Test Email
Content-Type: text/plain

This is a test email body.
It has multiple lines.
And some content."""
    
    def test_read_email_from_stdin(self):
        """Test reading email from stdin."""
        with patch('sys.stdin', StringIO(self.sample_email)):
            msg = self.classifier.read_email()
            self.assertEqual(msg['From'], 'sender@example.com')
            self.assertEqual(msg['Subject'], 'Test Email')
    
    def test_read_email_from_file(self):
        """Test reading email from file."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write(self.sample_email)
            f.flush()
            
            msg = self.classifier.read_email(f.name)
            self.assertEqual(msg['From'], 'sender@example.com')
            self.assertEqual(msg['Subject'], 'Test Email')
            
            os.unlink(f.name)
    
    def test_extract_fields(self):
        """Test field extraction from email."""
        with patch('sys.stdin', StringIO(self.sample_email)):
            msg = self.classifier.read_email()
            fields = self.classifier.extract_fields(msg)
            
            self.assertEqual(fields['from'], 'sender@example.com')
            self.assertEqual(fields['subject'], 'Test Email')
            self.assertIn('test email body', fields['body'].lower())
    
    def test_extract_fields_multipart(self):
        """Test field extraction from multipart email."""
        multipart_email = """From: sender@example.com
Subject: Multipart Test
Content-Type: multipart/alternative; boundary="boundary"

--boundary
Content-Type: text/plain

Plain text version
--boundary
Content-Type: text/html

<html>HTML version</html>
--boundary--"""
        
        with patch('sys.stdin', StringIO(multipart_email)):
            msg = self.classifier.read_email()
            fields = self.classifier.extract_fields(msg)
            
            self.assertEqual(fields['from'], 'sender@example.com')
            self.assertEqual(fields['subject'], 'Multipart Test')
            self.assertIn('Plain text version', fields['body'])
    
    def test_body_truncation(self):
        """Test that body is truncated to 1000 chars."""
        long_body = "x" * 2000
        long_email = f"""From: sender@example.com
Subject: Long Email

{long_body}"""
        
        with patch('sys.stdin', StringIO(long_email)):
            msg = self.classifier.read_email()
            fields = self.classifier.extract_fields(msg)
            
            self.assertEqual(len(fields['body']), 1000)
    
    def test_create_prompt(self):
        """Test prompt creation."""
        email_data = {
            'from': 'test@example.com',
            'subject': 'Test Subject',
            'body': 'Test body'
        }
        
        prompt = self.classifier.create_prompt(email_data)
        
        self.assertIn('test@example.com', prompt)
        self.assertIn('Test Subject', prompt)
        self.assertIn('Test body', prompt)
        self.assertIn('important', prompt.lower())
        self.assertIn('newsletter', prompt.lower())
    
    def test_classify_with_llm_client(self):
        """Test classification using the unified LLMClient."""
        # Configure the mock
        self.mock_llm_client_instance.complete.return_value = {"category": "important", "urgency": "high", "sender_type": "person"}

        # Run classification
        result = self.classifier.classify("test prompt")

        # Verify results
        self.assertEqual(result['category'], 'important')
        self.mock_llm_client_instance.complete.assert_called_once()
    
    def test_classify_with_error(self):
        """Test classification with LLM error."""
        # Configure the mock to raise an error
        self.mock_llm_client_instance.complete.side_effect = Exception("LLM error")

        # Run classification
        result = self.classifier.classify("test prompt")

        # Should return fallback
        self.assertEqual(result['category'], 'automated')
        self.assertIn('error', result)
    
    def test_dry_run(self):
        """Test dry run mode."""
        classifier = mail_classify.EmailClassifier(dry_run=True)
        
        with patch('sys.stdin', StringIO(self.sample_email)):
            with patch('sys.stderr', new_callable=StringIO) as mock_stderr:
                result = classifier.run()
                
                # Should output prompt to stderr
                stderr_output = mock_stderr.getvalue()
                self.assertIn('LLM PROMPT', stderr_output)
                self.assertIn('sender@example.com', stderr_output)
                
                # Should return default classification
                self.assertEqual(result['category'], 'automated')
    
    def test_debug_mode(self):
        """Test debug mode."""
        # Need to create a new classifier with debug=True
        self.mock_llm_client_instance.complete.return_value = {"category": "newsletter", "urgency": "low", "sender_type": "company"}
        classifier = mail_classify.EmailClassifier(debug=True)
        
        with patch('sys.stdin', StringIO(self.sample_email)):
            with patch('sys.stderr', new_callable=StringIO) as mock_stderr:
                result = classifier.run()
                
                # Should output prompt to stderr
                stderr_output = mock_stderr.getvalue()
                self.assertIn('LLM PROMPT', stderr_output)
                
                self.assertEqual(result['category'], 'newsletter')
    
    def test_missing_headers(self):
        """Test handling of missing email headers."""
        minimal_email = "This is just body text"
        
        with patch('sys.stdin', StringIO(minimal_email)):
            msg = self.classifier.read_email()
            fields = self.classifier.extract_fields(msg)
            
            self.assertEqual(fields['from'], '')
            self.assertEqual(fields['subject'], '')
            self.assertIn('This is just body text', fields['body'])

    def test_json_output(self):
        """Test that output is valid JSON."""
        self.mock_llm_client_instance.complete.return_value = {"category": "important", "urgency": "high", "sender_type": "person"}
        
        with patch('sys.stdin', StringIO(self.sample_email)):
            with patch('sys.stdout', new_callable=StringIO) as mock_stdout:
                result = self.classifier.run()
                
                output = mock_stdout.getvalue().strip()
                # Should be valid JSON
                parsed = json.loads(output)
                self.assertEqual(parsed['category'], 'important')
                self.assertEqual(parsed['urgency'], 'high')
                self.assertEqual(parsed['sender_type'], 'person')


if __name__ == '__main__':
    unittest.main()
