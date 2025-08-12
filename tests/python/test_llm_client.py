import unittest
import sys
import os
import json
import subprocess
import requests
from unittest.mock import patch, MagicMock

# Add src/python to path to allow importing llm_client and config
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src/python')))

from llm_client import get_llm_client, CmdClient, OpenAICompatibleClient

class MockMailConfig:
    """A mock MailConfig class for testing."""
    def __init__(self, config_dict):
        self._config = config_dict

    def get(self, section, option, fallback=None):
        return self._config.get(section, {}).get(option, fallback)

    def get_llm_config(self):
        return self._config.get('llm', {})

class TestLLMClient(unittest.TestCase):
    """Tests for the LLM clients."""

    @patch('src.python.llm_client.subprocess.run')
    def test_cmd_client_success(self, mock_run):
        """Test successful invocation of CmdClient."""
        print("\nTesting CmdClient success...")
        config = MockMailConfig({'llm': {'command': 'echo "test response"'}})
        client = CmdClient(config)
        
        mock_process = MagicMock()
        mock_process.stdout = "test response"
        mock_process.stderr = ""
        mock_process.returncode = 0
        mock_run.return_value = mock_process

        response = client.complete("prompt")
        mock_run.assert_called_once_with(
            'echo "test response"',
            shell=True,
            input="prompt",
            text=True,
            capture_output=True,
            check=True
        )
        self.assertEqual(response, "test response")
        print("✓ OK")

    @patch('src.python.llm_client.subprocess.run')
    def test_cmd_client_failure(self, mock_run):
        """Test failing invocation of CmdClient."""
        print("\nTesting CmdClient failure...")
        config = MockMailConfig({'llm': {'command': 'exit 1'}})
        client = CmdClient(config)

        mock_run.side_effect = subprocess.CalledProcessError(
            returncode=1, cmd='exit 1', stderr='error'
        )
        response = client.complete("prompt")
        self.assertIn("failed with exit code 1", json.loads(response)["error"])
        print("✓ OK")

    @patch('src.python.llm_client.requests.post')
    def test_openai_compatible_client_success(self, mock_post):
        """Test successful invocation of OpenAICompatibleClient."""
        print("\nTesting OpenAICompatibleClient success...")
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "choices": [{"message": {"content": "api response"}}]
        }
        mock_post.return_value = mock_response

        config_data = {
            'llm': {
                'type': 'openai',
                'model': 'gpt-4',
                'api_key': 'test_key',
                'api_url': 'https://api.openai.com/v1'
            }
        }
        config = MockMailConfig(config_data)
        client = OpenAICompatibleClient(config, 'openai')
        response = client.complete("prompt")

        self.assertEqual(response, "api response")
        mock_post.assert_called_once()
        # Check headers
        headers = mock_post.call_args.kwargs['headers']
        self.assertEqual(headers['Authorization'], 'Bearer test_key')
        print("✓ OK")

    @patch('src.python.llm_client.requests.post')
    def test_openrouter_client_headers(self, mock_post):
        """Test that OpenRouter client includes specific headers."""
        print("\nTesting OpenRouter client headers...")
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "choices": [{"message": {"content": "api response"}}]
        }
        mock_post.return_value = mock_response

        config_data = {
            'llm': {
                'type': 'openrouter',
                'model': 'some/model',
                'api_key': 'test_key'
            }
        }
        config = MockMailConfig(config_data)
        client = OpenAICompatibleClient(config, 'openrouter')
        client.complete("prompt")

        mock_post.assert_called_once()
        headers = mock_post.call_args.kwargs['headers']
        self.assertIn('HTTP-Referer', headers)
        self.assertIn('X-Title', headers)
        print("✓ OK")

    @patch('src.python.llm_client.requests.post')
    def test_api_client_request_failure(self, mock_post):
        """Test API client failure on request exception."""
        print("\nTesting API client request failure...")
        mock_post.side_effect = requests.exceptions.RequestException("Connection error")

        config_data = {'llm': {'type': 'openai', 'model': 'gpt-4', 'api_key': 'test_key'}}
        config = MockMailConfig(config_data)
        client = OpenAICompatibleClient(config, 'openai')
        response = client.complete("prompt")
        
        response_json = json.loads(response)
        self.assertIn("error", response_json)
        self.assertIn("API request failed", response_json["error"])
        print("✓ OK")

    def test_factory_function(self):
        """Test the get_llm_client factory function."""
        print("\nTesting LLM client factory...")
        # Test CmdClient
        config = MockMailConfig({'llm': {'type': 'cmd', 'command': 'echo "hi"'}})
        client = get_llm_client(config)
        self.assertIsInstance(client, CmdClient)

        # Test OpenAICompatibleClient for various types
        for llm_type in ['openai', 'ollama', 'openrouter', 'gpt4all']:
            with self.subTest(llm_type=llm_type):
                config_data = {'llm': {'type': llm_type, 'model': 'test-model'}}
                if llm_type in ['openai', 'openrouter']:
                    config_data['llm']['api_key'] = 'fake-key'
                
                config = MockMailConfig(config_data)
                client = get_llm_client(config)
                self.assertIsInstance(client, OpenAICompatibleClient)

        # Test unsupported type
        with self.assertRaises(ValueError):
            config = MockMailConfig({'llm': {'type': 'unsupported'}})
            get_llm_client(config)
        print("✓ OK")

    def test_config_validation(self):
        """Test configuration validation for clients."""
        print("\nTesting client configuration validation...")
        # OpenAI requires API key
        with self.assertRaisesRegex(ValueError, "API key is required for OpenAI"):
            config = MockMailConfig({'llm': {'type': 'openai', 'model': 'test'}})
            OpenAICompatibleClient(config, 'openai')

        # OpenRouter requires API key
        with self.assertRaisesRegex(ValueError, "API key is required for OpenRouter"):
            config = MockMailConfig({'llm': {'type': 'openrouter', 'model': 'test'}})
            OpenAICompatibleClient(config, 'openrouter')

        # API clients require model
        with self.assertRaisesRegex(ValueError, "Model is not configured"):
            config = MockMailConfig({'llm': {'type': 'ollama'}})
            OpenAICompatibleClient(config, 'ollama')
        
        # Cmd client requires command
        with self.assertRaisesRegex(ValueError, "LLM command is not configured"):
            config = MockMailConfig({'llm': {'type': 'cmd'}})
            CmdClient(config)
        print("✓ OK")

if __name__ == '__main__':
    # Add a bit more verbosity to the test output
    print("="*70)
    print("Running tests for llm_client.py")
    print("="*70)
    unittest.main(verbosity=0)
