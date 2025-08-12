import sys
import os
import subprocess
import requests
import json
from abc import ABC, abstractmethod
from typing import Dict, Any

# Ensure the script's directory is in the Python path to find other modules.
sys.path.append(os.path.dirname(os.path.realpath(__file__)))

from config import get_config, MailConfig

class LLMClient(ABC):
    """Abstract base class for LLM clients."""
    @abstractmethod
    def invoke(self, prompt: str) -> str:
        """Invoke the LLM with a given prompt and return the response."""
        pass

class CmdClient(LLMClient):
    """An LLM client that executes a shell command."""
    def __init__(self, config: MailConfig):
        self.command = config.get('llm', 'command')
        if not self.command:
            raise ValueError("LLM command is not configured for type 'cmd'.")

    def invoke(self, prompt: str) -> str:
        """Passes the prompt to the configured command via stdin."""
        try:
            process = subprocess.run(
                self.command,
                shell=True,
                input=prompt,
                text=True,
                capture_output=True,
                check=True
            )
            return process.stdout.strip()
        except subprocess.CalledProcessError as e:
            error_message = f"Command '{self.command}' failed with exit code {e.returncode}."
            if e.stderr:
                error_message += f" Stderr: {e.stderr.strip()}"
            # Return error as a JSON string to be consistent with other failures
            return json.dumps({"error": error_message})

class OpenAICompatibleClient(LLMClient):
    """A client for OpenAI-compatible APIs like OpenAI, Ollama, OpenRouter, and GPT4All."""
    def __init__(self, config: MailConfig, llm_type: str):
        self.llm_type = llm_type
        llm_config = config.get_llm_config()
        
        self.api_key = llm_config.get('api_key')
        self.api_url = llm_config.get('api_url')
        self.model = llm_config.get('model')

        if not self.model and self.llm_type != 'cmd':
            raise ValueError(f"Model is not configured for LLM type '{self.llm_type}'.")

        if self.llm_type == 'openai':
            if not self.api_url:
                self.api_url = "https://api.openai.com/v1"
            if not self.api_key:
                raise ValueError("API key is required for OpenAI.")
        elif self.llm_type == 'ollama':
            if not self.api_url:
                self.api_url = "http://localhost:11434/v1"
        elif self.llm_type == 'openrouter':
            self.api_url = "https://openrouter.ai/api/v1"
            if not self.api_key:
                raise ValueError("API key is required for OpenRouter.")
        elif self.llm_type == 'gpt4all':
            if not self.api_url:
                self.api_url = "http://localhost:4891/v1"
        
        if self.api_url and not self.api_url.endswith('/chat/completions'):
            self.api_url = self.api_url.rstrip('/') + '/chat/completions'

    def invoke(self, prompt: str) -> str:
        """Sends a request to the OpenAI-compatible API."""
        headers = {"Content-Type": "application/json"}
        if self.api_key:
            headers["Authorization"] = f"Bearer {self.api_key}"
        
        if self.llm_type == 'openrouter':
            # OpenRouter recommends these headers.
            # The user should ideally set their own repo URL.
            headers["HTTP-Referer"] = "https://github.com/user/mu-gnus"
            headers["X-Title"] = "mu-gnus"

        data = {
            "model": self.model,
            "messages": [{"role": "user", "content": prompt}],
            "stream": False
        }

        try:
            response = requests.post(self.api_url, headers=headers, json=data, timeout=60)
            response.raise_for_status()
            result = response.json()
            content = result.get('choices', [{}])[0].get('message', {}).get('content')
            if content:
                return content
            else:
                return json.dumps({"error": "No content in response", "response": result})
        except requests.exceptions.RequestException as e:
            return json.dumps({"error": f"API request failed: {e}"})
        except (KeyError, IndexError, json.JSONDecodeError) as e:
            return json.dumps({"error": f"Failed to parse API response: {e}", "response_text": response.text})

def get_llm_client(config: MailConfig) -> LLMClient:
    """Factory function to get the appropriate LLM client based on config."""
    llm_type = config.get('llm', 'type', 'cmd').lower()
    
    if llm_type == 'cmd':
        return CmdClient(config)
    elif llm_type in ['openai', 'ollama', 'openrouter', 'gpt4all']:
        return OpenAICompatibleClient(config, llm_type)
    else:
        raise ValueError(f"Unsupported LLM type: '{llm_type}'")

def main():
    """
    Command-line interface for the LLM client.
    Reads prompt from stdin and prints response to stdout.
    """
    if sys.stdin.isatty() and len(sys.argv) == 1:
        print(f"Usage: echo 'Your prompt' | python3 {sys.argv[0]}", file=sys.stderr)
        sys.exit(1)

    prompt = sys.stdin.read()
    
    try:
        # The get_config function will find the config file.
        config = get_config()
        client = get_llm_client(config)
        response = client.invoke(prompt)
        print(response)
    except Exception as e:
        # Print error as JSON for programmatic use, to stdout.
        print(json.dumps({"error": f"An error occurred: {str(e)}"}))
        sys.exit(1)

if __name__ == "__main__":
    main()
