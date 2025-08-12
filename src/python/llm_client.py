import sys
import os
import subprocess
import requests
import json
import hashlib
import argparse
from pathlib import Path
from abc import ABC, abstractmethod
from typing import Optional

# Ensure the script's directory is in the Python path to find other modules.
sys.path.append(os.path.dirname(os.path.realpath(__file__)))

from config import get_config, MailConfig

try:
    from gpt4all import GPT4All
except ImportError:
    GPT4All = None

class LLMClient(ABC):
    """Abstract base class for LLM clients."""
    def __init__(self, config: MailConfig, use_cache: bool = True):
        self.config = config
        self.use_cache = use_cache
        self.cache_path = None
        if self.use_cache:
            cache_dir = config.getpath('llm', 'cache_path', '~/.cache/mu-gnus/llm_cache')
            self.cache_path = Path(cache_dir).expanduser()
            self.cache_path.mkdir(parents=True, exist_ok=True)

    def _get_cache_key(self, prompt: str, system: Optional[str], model: str, json_output: bool) -> str:
        hasher = hashlib.sha256()
        hasher.update(prompt.encode('utf-8'))
        if system:
            hasher.update(system.encode('utf-8'))
        if model:
            hasher.update(model.encode('utf-8'))
        hasher.update(str(json_output).encode('utf-8'))
        return hasher.hexdigest()

    def _get_from_cache(self, key: str) -> Optional[str]:
        if not self.use_cache or not self.cache_path:
            return None
        cache_file = self.cache_path / key
        if cache_file.exists():
            return cache_file.read_text('utf-8')
        return None

    def _save_to_cache(self, key: str, response: str):
        if not self.use_cache or not self.cache_path:
            return
        cache_file = self.cache_path / key
        cache_file.write_text(response, 'utf-8')

    @abstractmethod
    def complete(self, prompt: str, system: Optional[str] = None, json_output: bool = False) -> str:
        """Invoke the LLM with a given prompt and return the response."""
        pass

class CmdClient(LLMClient):
    """An LLM client that executes a shell command."""
    def __init__(self, config: MailConfig, use_cache: bool = True):
        super().__init__(config, use_cache)
        self.command = self.config.get('llm', 'command')
        if not self.command:
            raise ValueError("LLM command is not configured for type 'cmd'.")

    def complete(self, prompt: str, system: Optional[str] = None, json_output: bool = False) -> str:
        cache_key = self._get_cache_key(prompt, system, self.command, json_output)
        cached_response = self._get_from_cache(cache_key)
        if cached_response:
            return cached_response

        try:
            process = subprocess.run(
                self.command, shell=True, input=prompt, text=True,
                capture_output=True, check=True
            )
            response = process.stdout.strip()
            self._save_to_cache(cache_key, response)
            return response
        except subprocess.CalledProcessError as e:
            error_message = f"Command '{self.command}' failed with exit code {e.returncode}."
            if e.stderr:
                error_message += f" Stderr: {e.stderr.strip()}"
            return json.dumps({"error": error_message})

class OpenAICompatibleClient(LLMClient):
    """A client for OpenAI-compatible APIs like OpenAI and OpenRouter."""
    def __init__(self, config: MailConfig, llm_type: str, use_cache: bool = True):
        super().__init__(config, use_cache)
        self.llm_type = llm_type
        llm_config = self.config.get_llm_config()
        
        self.api_key = llm_config.get('api_key')
        self.api_url = llm_config.get('api_url')
        self.model = llm_config.get('model')

        if not self.model:
            raise ValueError(f"Model is not configured for LLM type '{self.llm_type}'.")

        if self.llm_type == 'openai':
            if not self.api_url: self.api_url = "https://api.openai.com/v1"
            if not self.api_key: raise ValueError("API key is required for OpenAI.")
        elif self.llm_type == 'openrouter':
            self.api_url = "https://openrouter.ai/api/v1"
            if not self.api_key: raise ValueError("API key is required for OpenRouter.")
        
        if self.api_url and not self.api_url.endswith('/chat/completions'):
            self.api_url = self.api_url.rstrip('/') + '/chat/completions'

    def complete(self, prompt: str, system: Optional[str] = None, json_output: bool = False) -> str:
        cache_key = self._get_cache_key(prompt, system, self.model, json_output)
        cached_response = self._get_from_cache(cache_key)
        if cached_response: return cached_response

        headers = {"Content-Type": "application/json"}
        if self.api_key: headers["Authorization"] = f"Bearer {self.api_key}"
        
        if self.llm_type == 'openrouter':
            headers["HTTP-Referer"] = self.config.get('openrouter', 'referer', 'https://github.com/user/mu-gnus')
            headers["X-Title"] = self.config.get('openrouter', 'title', 'mu-gnus')

        messages = []
        if system: messages.append({"role": "system", "content": system})
        messages.append({"role": "user", "content": prompt})

        data = {"model": self.model, "messages": messages, "stream": False}
        if json_output and self.llm_type == 'openai':
            data["response_format"] = {"type": "json_object"}

        try:
            response = requests.post(self.api_url, headers=headers, json=data, timeout=60)
            response.raise_for_status()
            result = response.json()
            content = result.get('choices', [{}])[0].get('message', {}).get('content')
            if content:
                self._save_to_cache(cache_key, content)
                return content
            else:
                return json.dumps({"error": "No content in response", "response": result})
        except requests.exceptions.RequestException as e:
            return json.dumps({"error": f"API request failed: {e}"})
        except (KeyError, IndexError, json.JSONDecodeError) as e:
            return json.dumps({"error": f"Failed to parse API response: {e}", "response_text": response.text})

class OllamaClient(LLMClient):
    """A client for Ollama's /api/generate endpoint."""
    def __init__(self, config: MailConfig, use_cache: bool = True):
        super().__init__(config, use_cache)
        llm_config = self.config.get_llm_config()
        self.api_url = llm_config.get('api_url', "http://localhost:11434")
        self.model = llm_config.get('model')

        if not self.model: raise ValueError("Model is not configured for LLM type 'ollama'.")
        
        if not self.api_url.endswith('/api/generate'):
            self.api_url = self.api_url.rstrip('/') + '/api/generate'

    def complete(self, prompt: str, system: Optional[str] = None, json_output: bool = False) -> str:
        cache_key = self._get_cache_key(prompt, system, self.model, json_output)
        cached_response = self._get_from_cache(cache_key)
        if cached_response: return cached_response

        data = {"model": self.model, "prompt": prompt, "stream": False}
        if system: data["system"] = system
        if json_output: data["format"] = "json"

        try:
            response = requests.post(self.api_url, json=data, timeout=60)
            response.raise_for_status()
            result = response.json()
            content = result.get('response')
            if content:
                self._save_to_cache(cache_key, content)
                return content.strip()
            else:
                return json.dumps({"error": "No content in response", "response": result})
        except requests.exceptions.RequestException as e:
            return json.dumps({"error": f"API request failed: {e}"})
        except json.JSONDecodeError as e:
            return json.dumps({"error": f"Failed to parse API response: {e}", "response_text": response.text})

class GPT4AllClient(LLMClient):
    """A client for GPT4All using the Python bindings."""
    def __init__(self, config: MailConfig, use_cache: bool = True):
        super().__init__(config, use_cache)
        if not GPT4All:
            raise ImportError("GPT4All library not found. Please install with 'pip install gpt4all'.")
        
        llm_config = self.config.get_llm_config()
        self.model_name = llm_config.get('model')
        if not self.model_name:
            raise ValueError("Model is not configured for LLM type 'gpt4all'.")
        
        model_path = self.config.getpath('gpt4all', 'model_path')
        try:
            self.model = GPT4All(model_name=self.model_name, model_path=str(model_path) if model_path else None)
        except Exception as e:
            raise RuntimeError(f"Failed to initialize GPT4All model: {e}")

    def complete(self, prompt: str, system: Optional[str] = None, json_output: bool = False) -> str:
        if json_output:
            prompt += "\n\nRespond with JSON."

        cache_key = self._get_cache_key(prompt, system, self.model_name, json_output)
        cached_response = self._get_from_cache(cache_key)
        if cached_response: return cached_response

        try:
            with self.model.chat_session(system_prompt=system if system else ''):
                response = self.model.generate(prompt, max_tokens=2048)
            
            self._save_to_cache(cache_key, response)
            return response.strip()
        except Exception as e:
            return json.dumps({"error": f"GPT4All generation failed: {e}"})

def get_llm_client(config: MailConfig, use_cache: bool = True) -> LLMClient:
    """Factory function to get the appropriate LLM client based on config."""
    llm_type = config.get('llm', 'type', 'cmd').lower()
    
    if llm_type == 'cmd':
        return CmdClient(config, use_cache)
    elif llm_type == 'openai':
        return OpenAICompatibleClient(config, 'openai', use_cache)
    elif llm_type == 'openrouter':
        return OpenAICompatibleClient(config, 'openrouter', use_cache)
    elif llm_type == 'ollama':
        return OllamaClient(config, use_cache)
    elif llm_type == 'gpt4all':
        return GPT4AllClient(config, use_cache)
    else:
        raise ValueError(f"Unsupported LLM type: '{llm_type}'")

def main():
    """
    Command-line interface for the LLM client.
    Reads prompt from stdin and prints response to stdout.
    """
    parser = argparse.ArgumentParser(description="LLM Client CLI")
    parser.add_argument('--no-cache', action='store_true', help="Disable caching for this run.")
    parser.add_argument('--system', help="System prompt to use.")
    parser.add_argument('--json', action='store_true', help="Request JSON output if supported.")
    args = parser.parse_args()

    if sys.stdin.isatty():
        print(f"Usage: echo 'Your prompt' | python3 {sys.argv[0]} [options]", file=sys.stderr)
        parser.print_help(file=sys.stderr)
        sys.exit(1)

    prompt = sys.stdin.read()
    
    try:
        config = get_config()
        client = get_llm_client(config, use_cache=not args.no_cache)
        response = client.complete(prompt, system=args.system, json_output=args.json)
        print(response)
    except Exception as e:
        print(json.dumps({"error": f"An error occurred: {str(e)}"}))
        sys.exit(1)

if __name__ == "__main__":
    main()
