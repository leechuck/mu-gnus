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

# Add src directory to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../src/python'))

import importlib.util
spec = importlib.util.spec_from_file_location("mail_classify", 
                                               os.path.join(os.path.dirname(__file__), 
                                                          '../../src/python/mail-classify.py'))
mail_classify = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mail_classify)


class TestEmailClassifier(unittest.TestCase):
    
    def setUp(self):
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
            self.assertIn('test email body', fields['body_preview'].lower())
    
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
            self.assertIn('Plain text version', fields['body_preview'])
    
    def test_body_truncation(self):
        """Test that body is truncated to 500 chars."""
        long_body = "x" * 1000
        long_email = f"""From: sender@example.com
Subject: Long Email

{long_body}"""
        
        with patch('sys.stdin', StringIO(long_email)):
            msg = self.classifier.read_email()
            fields = self.classifier.extract_fields(msg)
            
            self.assertEqual(len(fields['body_preview']), 500)
    
    def test_create_prompt(self):
        """Test prompt creation."""
        email_data = {
            'from': 'test@example.com',
            'subject': 'Test Subject',
            'body_preview': 'Test body'
        }
        
        prompt = self.classifier.create_prompt(email_data)
        
        self.assertIn('test@example.com', prompt)
        self.assertIn('Test Subject', prompt)
        self.assertIn('Test body', prompt)
        self.assertIn('important', prompt.lower())
        self.assertIn('newsletter', prompt.lower())
    
    @patch('requests.post')
    def test_call_ollama(self, mock_post):
        """Test Ollama API call."""
        mock_response = MagicMock()
        mock_response.json.return_value = {'response': 'newsletter'}
        mock_post.return_value = mock_response
        
        with patch.dict(os.environ, {'MAIL_LLM_TYPE': 'ollama'}):
            result = self.classifier.call_ollama("test prompt")
            self.assertEqual(result, 'newsletter')
            
            mock_post.assert_called_once()
            call_args = mock_post.call_args
            self.assertIn('localhost:11434', call_args[0][0])
    
    @patch('requests.post')
    def test_call_openai(self, mock_post):
        """Test OpenAI API call."""
        mock_response = MagicMock()
        mock_response.json.return_value = {
            'choices': [{'message': {'content': 'important'}}]
        }
        mock_post.return_value = mock_response
        
        with patch.dict(os.environ, {'OPENAI_API_KEY': 'test-key'}):
            result = self.classifier.call_openai("test prompt")
            self.assertEqual(result, 'important')
            
            mock_post.assert_called_once()
            call_args = mock_post.call_args
            self.assertIn('api.openai.com', call_args[0][0])
    
    @patch('subprocess.run')
    def test_call_command(self, mock_run):
        """Test external command call."""
        mock_run.return_value = MagicMock(stdout='social')
        
        with patch.dict(os.environ, {'MAIL_LLM_CMD': 'echo social'}):
            result = self.classifier.call_command("test prompt")
            self.assertEqual(result, 'social')
    
    def test_validate_classification_valid(self):
        """Test validation of valid classifications."""
        valid_classes = ['important', 'newsletter', 'social', 'automated']
        
        for valid in valid_classes:
            result = self.classifier.validate_classification(valid)
            self.assertEqual(result, valid)
            
            # Test with extra whitespace
            result = self.classifier.validate_classification(f"  {valid}  \n")
            self.assertEqual(result, valid)
            
            # Test with uppercase
            result = self.classifier.validate_classification(valid.upper())
            self.assertEqual(result, valid)
    
    def test_validate_classification_invalid(self):
        """Test validation of invalid classifications."""
        invalid_classes = ['spam', 'urgent', 'random', '']
        
        for invalid in invalid_classes:
            result = self.classifier.validate_classification(invalid)
            self.assertEqual(result, 'automated')
    
    def test_validate_classification_partial(self):
        """Test validation when valid class is part of response."""
        result = self.classifier.validate_classification("This is important email")
        self.assertEqual(result, 'important')
        
        result = self.classifier.validate_classification("newsletter: yes")
        self.assertEqual(result, 'newsletter')
    
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
                self.assertEqual(result, 'automated')
    
    def test_debug_mode(self):
        """Test debug mode."""
        classifier = mail_classify.EmailClassifier(debug=True)
        
        with patch('sys.stdin', StringIO(self.sample_email)):
            with patch('sys.stderr', new_callable=StringIO) as mock_stderr:
                with patch.object(classifier, 'classify', return_value='newsletter'):
                    result = classifier.run()
                    
                    # Should output prompt to stderr
                    stderr_output = mock_stderr.getvalue()
                    self.assertIn('LLM PROMPT', stderr_output)
                    
                    self.assertEqual(result, 'newsletter')
    
    @patch('requests.post')
    def test_integration_ollama(self, mock_post):
        """Test full integration with Ollama."""
        mock_response = MagicMock()
        mock_response.json.return_value = {'response': 'newsletter'}
        mock_post.return_value = mock_response
        
        with patch.dict(os.environ, {'MAIL_LLM_TYPE': 'ollama'}):
            with patch('sys.stdin', StringIO(self.sample_email)):
                with patch('sys.stdout', new_callable=StringIO) as mock_stdout:
                    self.classifier.run()
                    
                    output = mock_stdout.getvalue().strip()
                    self.assertEqual(output, 'newsletter')
    
    def test_missing_headers(self):
        """Test handling of missing email headers."""
        minimal_email = "This is just body text"
        
        with patch('sys.stdin', StringIO(minimal_email)):
            msg = self.classifier.read_email()
            fields = self.classifier.extract_fields(msg)
            
            self.assertEqual(fields['from'], '')
            self.assertEqual(fields['subject'], '')
            self.assertIn('This is just body text', fields['body_preview'])


if __name__ == '__main__':
    unittest.main()
