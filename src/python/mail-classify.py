#!/usr/bin/env python3
"""
Email classification tool that uses LLM to categorize emails.
Reads email from stdin or file, extracts key fields, and classifies.
"""

import sys
import os
import argparse
import json
import email
from email import policy
from email.parser import Parser
import subprocess
from pathlib import Path

# Try to import the unified LLM client
try:
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    from llm_client import LLMClient
    HAS_LLM_CLIENT = True
except ImportError:
    HAS_LLM_CLIENT = False


class EmailClassifier:
    def __init__(self, debug=False, dry_run=False):
        self.debug = debug
        self.dry_run = dry_run
        
        # Try to use unified LLM client if available
        self.llm_client = None
        if HAS_LLM_CLIENT and not dry_run:
            try:
                # Initialize with caching enabled by default
                self.llm_client = LLMClient(no_cache=False)
                if debug:
                    print(f"Using unified LLM client with backend: {self.llm_client.backend}", file=sys.stderr)
            except Exception as e:
                if debug:
                    print(f"Failed to initialize LLM client: {e}", file=sys.stderr)
                    print("Falling back to legacy methods", file=sys.stderr)
        
        # Legacy configuration for backward compatibility
        self.llm_type = os.environ.get('MAIL_LLM_TYPE', 'ollama')
        
    def read_email(self, source=None):
        """Read email from file or stdin."""
        if source and source != '-':
            with open(source, 'r') as f:
                content = f.read()
        else:
            content = sys.stdin.read()
        
        # Parse email
        parser = Parser(policy=policy.default)
        msg = parser.parsestr(content)
        
        return msg
    
    def extract_fields(self, msg):
        """Extract From, Subject, and first 500 chars of body."""
        from_addr = msg.get('From', '')
        subject = msg.get('Subject', '')
        
        # Get body
        body = ''
        if msg.is_multipart():
            for part in msg.walk():
                if part.get_content_type() == 'text/plain':
                    try:
                        body = part.get_content()
                        break
                    except:
                        pass
        else:
            try:
                body = msg.get_content()
            except:
                body = str(msg.get_payload())
        
        # Truncate body to first 500 chars
        body = body[:500] if body else ''
        
        return {
            'from': from_addr,
            'subject': subject,
            'body_preview': body
        }
    
    def create_prompt(self, email_data):
        """Create classification prompt for LLM."""
        prompt = f"""Classify this email into exactly one category: important, newsletter, social, or automated.

From: {email_data['from']}
Subject: {email_data['subject']}
Body preview: {email_data['body_preview']}

Respond with only a single word: important, newsletter, social, or automated."""
        
        return prompt
    
    def classify(self, prompt):
        """Call appropriate LLM based on configuration."""
        if self.dry_run:
            return "automated"
        
        # Try unified LLM client first
        if self.llm_client:
            try:
                system_prompt = "You are an email classifier. Respond with only one word: important, newsletter, social, or automated."
                response = self.llm_client.complete(prompt, system=system_prompt)
                if response:
                    return response.strip().lower()
            except Exception as e:
                if self.debug:
                    print(f"LLM client error: {e}", file=sys.stderr)
                    print("Falling back to legacy methods", file=sys.stderr)
        
        # Fall back to legacy methods
        if self.llm_type == 'ollama':
            return self.call_ollama(prompt)
        elif self.llm_type == 'gpt4all':
            return self.call_gpt4all(prompt)
        elif self.llm_type == 'openai':
            return self.call_openai(prompt)
        elif self.llm_type == 'cmd':
            return self.call_command(prompt)
        else:
            if self.debug:
                print(f"Unknown LLM type: {self.llm_type}", file=sys.stderr)
            return "automated"
    
    def call_ollama(self, prompt):
        """Call Ollama API (legacy method for backward compatibility)."""
        try:
            import requests
        except ImportError:
            if self.debug:
                print("requests library not installed", file=sys.stderr)
            return "automated"
        
        url = "http://localhost:11434/api/generate"
        
        payload = {
            "model": os.environ.get('MAIL_LLM_MODEL', 'llama2'),
            "prompt": prompt,
            "stream": False,
            "options": {
                "temperature": 0.1
            }
        }
        
        try:
            response = requests.post(url, json=payload)
            response.raise_for_status()
            result = response.json()
            return result.get('response', '').strip().lower()
        except Exception as e:
            if self.debug:
                print(f"Error calling Ollama: {e}", file=sys.stderr)
            return "automated"
    
    def call_gpt4all(self, prompt):
        """Call GPT4All API (legacy method for backward compatibility)."""
        # GPT4All typically runs on port 4891
        try:
            import requests
        except ImportError:
            if self.debug:
                print("requests library not installed", file=sys.stderr)
            return "automated"
        
        host = os.environ.get('GPT4ALL_HOST', 'localhost')
        port = os.environ.get('GPT4ALL_PORT', '4891')
        url = f"http://{host}:{port}/v1/completions"
        
        payload = {
            "model": os.environ.get('MAIL_LLM_MODEL', 'gpt4all-falcon-q4_0'),
            "prompt": prompt,
            "max_tokens": 10,
            "temperature": 0.1,
            "top_p": 0.9,
            "stream": False
        }
        
        try:
            response = requests.post(url, json=payload)
            response.raise_for_status()
            result = response.json()
            # GPT4All returns completions in 'choices' array
            if 'choices' in result and len(result['choices']) > 0:
                text = result['choices'][0].get('text', '').strip().lower()
                return text
            return "automated"
        except Exception as e:
            if self.debug:
                print(f"Error calling GPT4All: {e}", file=sys.stderr)
            return "automated"
    
    def call_openai(self, prompt):
        """Call OpenAI API (legacy method for backward compatibility)."""
        api_key = os.environ.get('OPENAI_API_KEY')
        if not api_key:
            if self.debug:
                print("OPENAI_API_KEY not set", file=sys.stderr)
            return "automated"
        
        try:
            import requests
        except ImportError:
            if self.debug:
                print("requests library not installed", file=sys.stderr)
            return "automated"
        
        url = "https://api.openai.com/v1/chat/completions"
        
        headers = {
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json"
        }
        
        payload = {
            "model": os.environ.get('MAIL_LLM_MODEL', 'gpt-3.5-turbo'),
            "messages": [
                {"role": "system", "content": "You are an email classifier. Respond with only: important, newsletter, social, or automated."},
                {"role": "user", "content": prompt}
            ],
            "temperature": 0.1,
            "max_tokens": 10
        }
        
        try:
            response = requests.post(url, headers=headers, json=payload)
            response.raise_for_status()
            result = response.json()
            content = result['choices'][0]['message']['content'].strip().lower()
            return content
        except Exception as e:
            if self.debug:
                print(f"Error calling OpenAI: {e}", file=sys.stderr)
            return "automated"
    
    def call_command(self, prompt):
        """Call external command (legacy method for backward compatibility)."""
        cmd = os.environ.get('MAIL_LLM_CMD')
        if not cmd:
            if self.debug:
                print("MAIL_LLM_CMD not set", file=sys.stderr)
            return "automated"
        
        try:
            result = subprocess.run(
                cmd,
                shell=True,
                input=prompt,
                text=True,
                capture_output=True,
                timeout=30
            )
            return result.stdout.strip().lower()
        except Exception as e:
            if self.debug:
                print(f"Error calling command: {e}", file=sys.stderr)
            return "automated"
    
    def validate_classification(self, classification):
        """Ensure classification is valid."""
        valid = ['important', 'newsletter', 'social', 'automated']
        
        # Clean up the response
        classification = classification.strip().lower()
        
        # Check if any valid classification is in the response
        for valid_class in valid:
            if valid_class in classification:
                return valid_class
        
        # Default to automated if invalid
        return "automated"
    
    def run(self, source=None):
        """Main classification workflow."""
        # Read and parse email
        msg = self.read_email(source)
        
        # Extract fields
        email_data = self.extract_fields(msg)
        
        # Create prompt
        prompt = self.create_prompt(email_data)
        
        # Debug output
        if self.debug or self.dry_run:
            print("=== LLM PROMPT ===", file=sys.stderr)
            print(prompt, file=sys.stderr)
            print("==================", file=sys.stderr)
        
        # Classify
        classification = self.classify(prompt)
        
        # Validate and output
        result = self.validate_classification(classification)
        print(result)
        
        return result


def main():
    parser = argparse.ArgumentParser(description='Classify emails using LLM')
    parser.add_argument('file', nargs='?', help='Email file to classify (default: stdin)')
    parser.add_argument('--debug', action='store_true', help='Show LLM prompt and debug info')
    parser.add_argument('--dry-run', action='store_true', help='Show prompt without calling LLM')
    
    args = parser.parse_args()
    
    classifier = EmailClassifier(debug=args.debug, dry_run=args.dry_run)
    classifier.run(args.file)


if __name__ == '__main__':
    main()
