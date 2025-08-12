import unittest
import sys
import os
import json
import subprocess
import tempfile
import shutil
from pathlib import Path
from unittest.mock import patch, MagicMock, ANY

# Add src/python to path to allow importing llm_client and config
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src/python')))

# Mock gpt4all before importing llm_client
mock_gpt4all_module = MagicMock()
sys.modules['gpt4all'] = mock_gpt4all_module

from llm_client import get_llm_client, CmdClient, OpenAICompatibleClient, OllamaClient, GPT4AllClient

class MockMailConfig:
    """A mock MailConfig class for testing."""
    def __init__(self, config_dict):
        self._config = config_dict

    def get(self, section, option, fallback=None):
        return self._config.get(section, {}).get(option, fallback)

    def getpath(self, section, option, fallback=''):
        path_str = self._config.get(section, {}).get(option, fallback)
        return Path(path_str) if path_str else Path(fallback)

    def get_llm_config(self):
        return self._config.get('llm', {})

class TestLLMClient(unittest.TestCase):
    """Tests for the LLM clients."""

    def setUp(self):
        self.temp_dir = tempfile.mkdtemp()
        self.cache_dir = os.path.join(self.temp_dir, 'cache')
        os.makedirs(self.cache_dir)

    def tearDown(self):
        shutil.rmtree(self.temp_dir)

    def _get_config(self, llm_config):
        config = {'llm': llm_config}
        if 'cache_path' not in config['llm']:
            config['llm']['cache_path'] = self.cache_dir
        return MockMailConfig(config)

    @patch('src.python.llm_client.subprocess.run')
    def test_cmd_client(self, mock_run):
        """Test CmdClient."""
        config = self._get_config({'type': 'cmd', 'command': 'echo "test response"'})
        client = get_llm_client(config)
        self.assertIsInstance(client, CmdClient)

        mock_process = MagicMock(stdout="test response", stderr="", returncode=0)
        mock_run.return_value = mock_process

        response = client.complete("prompt")
        mock_run.assert_called_once_with(
            'echo "test response"', shell=True, input="prompt", text=True,
            capture_output=True, check=True
        )
        self.assertEqual(response, "test response")

    @patch('src.python.llm_client.requests.post')
    def test_openai_client(self, mock_post):
        """Test OpenAICompatibleClient for OpenAI."""
        config = self._get_config({
            'type': 'openai', 'model': 'gpt-4', 'api_key': 'test_key'
        })
        client = get_llm_client(config)
        self.assertIsInstance(client, OpenAICompatibleClient)

        mock_post.return_value = MagicMock(
            status_code=200,
            json=lambda: {"choices": [{"message": {"content": "api response"}}]}
        )

        response = client.complete("prompt", system="system prompt", json_output=True)
        self.assertEqual(response, "api response")
        mock_post.assert_called_once()
        
        kwargs = mock_post.call_args.kwargs
        self.assertIn('Bearer test_key', kwargs['headers']['Authorization'])
        self.assertEqual(kwargs['json']['model'], 'gpt-4')
        self.assertEqual(kwargs['json']['messages'][0]['role'], 'system')
        self.assertEqual(kwargs['json']['messages'][0]['content'], 'system prompt')
        self.assertEqual(kwargs['json']['response_format'], {'type': 'json_object'})

    @patch('src.python.llm_client.requests.post')
    def test_openrouter_client(self, mock_post):
        """Test OpenAICompatibleClient for OpenRouter."""
        config = self._get_config({
            'type': 'openrouter', 'model': 'some/model', 'api_key': 'test_key'
        })
        client = get_llm_client(config)
        self.assertIsInstance(client, OpenAICompatibleClient)

        mock_post.return_value = MagicMock(
            status_code=200,
            json=lambda: {"choices": [{"message": {"content": "api response"}}]}
        )
        client.complete("prompt")
        headers = mock_post.call_args.kwargs['headers']
        self.assertIn('HTTP-Referer', headers)
        self.assertIn('X-Title', headers)

    @patch('src.python.llm_client.requests.post')
    def test_ollama_client(self, mock_post):
        """Test OllamaClient."""
        config = self._get_config({'type': 'ollama', 'model': 'llama2'})
        client = get_llm_client(config)
        self.assertIsInstance(client, OllamaClient)

        mock_post.return_value = MagicMock(
            status_code=200, json=lambda: {"response": "ollama says hi"}
        )
        response = client.complete("prompt", system="system prompt", json_output=True)
        self.assertEqual(response, "ollama says hi")

        kwargs = mock_post.call_args.kwargs
        self.assertEqual(kwargs['json']['model'], 'llama2')
        self.assertEqual(kwargs['json']['system'], 'system prompt')
        self.assertEqual(kwargs['json']['format'], 'json')

    def test_gpt4all_client(self):
        """Test GPT4AllClient."""
        if 'gpt4all' in sys.modules:
            mock_gpt4all_instance = MagicMock()
            mock_gpt4all_instance.generate.return_value = "gpt4all says hi"
            
            mock_chat_session = MagicMock()
            mock_chat_session.__enter__.return_value = None
            mock_chat_session.__exit__.return_value = (None, None, None)
            mock_gpt4all_instance.chat_session.return_value = mock_chat_session

            mock_gpt4all_module.GPT4All.return_value = mock_gpt4all_instance

            config = self._get_config({'type': 'gpt4all', 'model': 'fake-model.gguf'})
            client = get_llm_client(config)
            self.assertIsInstance(client, GPT4AllClient)

            response = client.complete("prompt", system="system prompt")
            self.assertEqual(response, "gpt4all says hi")
            mock_gpt4all_instance.generate.assert_called_with("prompt", max_tokens=ANY)
            mock_gpt4all_instance.chat_session.assert_called_with(system_prompt="system prompt")

    def test_factory_and_validation(self):
        """Test factory function and configuration validation."""
        with self.assertRaises(ValueError):
            get_llm_client(self._get_config({'type': 'unsupported'}))

        with self.assertRaises(ValueError):
            get_llm_client(self._get_config({'type': 'openai', 'model': 'test'}))

        with self.assertRaises(ValueError):
            get_llm_client(self._get_config({'type': 'ollama'}))

    def test_caching(self):
        """Test response caching."""
        with patch('src.python.llm_client.requests.post') as mock_post:
            mock_post.return_value = MagicMock(
                status_code=200,
                json=lambda: {"choices": [{"message": {"content": "api response"}}]}
            )
            config = self._get_config({'type': 'openai', 'model': 'gpt-4', 'api_key': 'test_key'})
            
            client_cache = get_llm_client(config, use_cache=True)
            res1 = client_cache.complete("prompt")
            self.assertEqual(res1, "api response")
            self.assertEqual(mock_post.call_count, 1)
            
            res2 = client_cache.complete("prompt")
            self.assertEqual(res2, "api response")
            self.assertEqual(mock_post.call_count, 1)

            client_no_cache = get_llm_client(config, use_cache=False)
            res3 = client_no_cache.complete("prompt")
            self.assertEqual(res3, "api response")
            self.assertEqual(mock_post.call_count, 2)

            res4 = client_no_cache.complete("prompt")
            self.assertEqual(res4, "api response")
            self.assertEqual(mock_post.call_count, 3)

if __name__ == '__main__':
    unittest.main(verbosity=2)
