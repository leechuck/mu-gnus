import unittest
from unittest.mock import patch, MagicMock
import os
import sys
import json
import tempfile

# Add src/python to path to allow importing mail_classify
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src/python')))

# Mock llm_client before importing mail_classify
# This avoids ImportError if llm_client or its dependencies are not installed
mock_llm_client_module = MagicMock()
sys.modules['llm_client'] = mock_llm_client_module

# Import mail_classify after mocking llm_client
import mail_classify

class TestNewEmailClassifier(unittest.TestCase):

    def setUp(self):
        self.sample_email = """From: sender@example.com
Subject: Test Subject
Content-Type: text/plain

This is a test body.
"""
        # Mock the LLMClient class from the mocked module
        self.mock_llm_client_instance = MagicMock()
        mock_llm_client_module.LLMClient.return_value = self.mock_llm_client_instance
        
        # Reset mocks before each test
        self.mock_llm_client_instance.reset_mock()
        mock_llm_client_module.LLMClient.reset_mock()

    @patch('sys.stdin')
    def test_classification_json_output(self, mock_stdin):
        """Test the main classification workflow with JSON output."""
        # Arrange
        mock_stdin.read.return_value = self.sample_email
        
        expected_response = {"category": "important", "urgency": "high", "sender_type": "person"}
        self.mock_llm_client_instance.complete.return_value = expected_response

        # We need to mock Path.exists to find the default prompt
        with patch('pathlib.Path.exists') as mock_exists, \
             patch('pathlib.Path.read_text') as mock_read_text:
            
            mock_exists.return_value = True
            mock_read_text.return_value = "From: {from}, Subject: {subject}, Body: {body}"
            
            # We need to re-initialize the classifier inside the patch context
            classifier = mail_classify.EmailClassifier(debug=False)

            # Act
            with patch('sys.stdout', new_callable=unittest.mock.StringIO) as mock_stdout:
                result = classifier.run()

        # Assert
        self.mock_llm_client_instance.complete.assert_called_once()
        
        # Check that the output is the expected JSON string
        output = mock_stdout.getvalue().strip()
        self.assertEqual(json.loads(output), expected_response)
        
        # Check that the run method returns the parsed dict
        self.assertEqual(result, expected_response)

    def test_custom_prompt_file(self):
        """Test that a custom prompt file is loaded and used."""
        custom_prompt = "Custom prompt with {from}, {subject}, and {body}"
        
        with tempfile.NamedTemporaryFile(mode='w+', delete=False, suffix=".txt") as tmp:
            tmp.write(custom_prompt)
            prompt_path = tmp.name

        try:
            # Arrange
            self.mock_llm_client_instance.complete.return_value = {"category": "automated", "urgency": "low", "sender_type": "system"}
            classifier = mail_classify.EmailClassifier(prompt_file=prompt_path)
            
            email_data = {
                'from': 'test@test.com',
                'subject': 'sub',
                'body': 'body'
            }

            # Act
            prompt = classifier.create_prompt(email_data)

            # Assert
            self.assertEqual(prompt, "Custom prompt with test@test.com, sub, and body")

        finally:
            os.remove(prompt_path)

if __name__ == '__main__':
    unittest.main()
