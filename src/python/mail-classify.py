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
    from config import get_config, MailConfig
    HAS_LLM_CLIENT = True
except ImportError:
    HAS_LLM_CLIENT = False


class EmailClassifier:
    def __init__(self, config: MailConfig, debug=False, dry_run=False, prompt_file=None):
        self.config = config
        self.debug = debug
        self.dry_run = dry_run
        
        # Use config values, with command-line arg for prompt_file as override
        prompt_path_override = Path(prompt_file) if prompt_file else None
        self.prompt_template = self._load_prompt_template(prompt_path_override)
        
        self.llm_type = self.config.get('llm', 'type')
        self.llm_cmd = self.config.get('llm', 'command')
        self.max_body_length = self.config.get_max_body_length()

        self.llm_client = None
        if not dry_run and self.llm_type != "cmd":
            if not HAS_LLM_CLIENT:
                raise ImportError("llm_client.py not found, which is required for classification.")
            try:
                self.llm_client = LLMClient(no_cache=False)
                if debug:
                    print(f"Using unified LLM client with backend: {self.llm_client.backend}", file=sys.stderr)
            except Exception as e:
                # Re-raise as a more informative error
                raise RuntimeError(f"Failed to initialize LLM client: {e}") from e

    def _load_prompt_template(self, prompt_file_override=None):
        """Load prompt template from file or use default."""
        # Priority: 1. command-line override, 2. config file, 3. hardcoded
        if prompt_file_override and prompt_file_override.exists():
            return prompt_file_override.read_text()
            
        config_prompt_file = self.config.get_prompt_file()
        if config_prompt_file and config_prompt_file.exists():
            return config_prompt_file.read_text()
        
        # Fallback hardcoded prompt
        return """Classify this email into ONE category:
- important: requires action or response
- newsletter: promotional, marketing, updates
- social: personal, casual conversations
- automated: system notifications, receipts
- spam: unwanted, suspicious

Also extract:
- urgency: high|medium|low
- sender_type: person|company|system

Email:
From: {from}
Subject: {subject}
Body (first 1000 chars): {body}

Respond in JSON: {"category": "...", "urgency": "...", "sender_type": "..."}
"""
        
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
        """Extract From, Subject, and first 1000 chars of body."""
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
        
        # Truncate body
        body = body[:self.max_body_length] if body else ''
        
        return {
            'from': from_addr,
            'subject': subject,
            'body': body
        }
    
    def create_prompt(self, email_data):
        """Create classification prompt for LLM."""
        # We use .replace() because '{from}' is not a valid .format() placeholder
        return self.prompt_template \
            .replace('{from}', email_data['from']) \
            .replace('{subject}', email_data['subject']) \
            .replace('{body}', email_data['body'])
    
    def classify(self, prompt):
        """Call LLM and get classification result as JSON."""
        if self.llm_type == "cmd":
            cmd = self.llm_cmd
            if not cmd:
                return {"category": "automated", "urgency": "low", "sender_type": "system", "error": "LLM type is 'cmd' but 'command' is not set in config.ini"}
            try:
                proc = subprocess.run(cmd, shell=True, check=True, capture_output=True, text=True)
                return json.loads(proc.stdout)
            except (subprocess.CalledProcessError, json.JSONDecodeError) as e:
                error_details = str(e)
                if hasattr(e, 'stderr') and e.stderr:
                    error_details += f" | stderr: {e.stderr.strip()}"
                return {"category": "automated", "urgency": "low", "sender_type": "system", "error": error_details}

        if self.dry_run:
            return {"category": "automated", "urgency": "low", "sender_type": "system"}

        system_prompt = "You are an email classifier. Respond only with the requested JSON object."
        
        try:
            response = self.llm_client.complete(prompt, system=system_prompt, json_mode=True)
            if not response:
                raise ValueError("LLM returned an empty response.")
            
            # The llm_client should ideally return a dict when json_mode=True,
            # but if it returns a string, we parse it.
            if isinstance(response, str):
                result = json.loads(response)
            else:
                result = response

            # Basic validation
            if not all(k in result for k in ['category', 'urgency', 'sender_type']):
                raise ValueError(f"LLM response missing required keys: {result}")
            
            return result

        except Exception as e:
            if self.debug:
                print(f"LLM classification error: {e}", file=sys.stderr)
            # Fallback to a default JSON structure on error
            return {"category": "automated", "urgency": "low", "sender_type": "system", "error": str(e)}
    
    def run(self, source=None):
        """Main classification workflow."""
        msg = self.read_email(source)
        email_data = self.extract_fields(msg)
        prompt = self.create_prompt(email_data)
        
        if self.debug or self.dry_run:
            print("=== LLM PROMPT ===", file=sys.stderr)
            print(prompt, file=sys.stderr)
            print("==================", file=sys.stderr)
        
        classification_result = self.classify(prompt)
        
        # Output the full JSON result. This will be consumed by mail-process.
        output_json = json.dumps(classification_result, separators=(',', ':'))
        print(output_json)
        
        return classification_result


def main():
    parser = argparse.ArgumentParser(description='Classify emails using LLM')
    parser.add_argument('file', nargs='?', help='Email file to classify (default: stdin)')
    parser.add_argument('--debug', action='store_true', help='Show LLM prompt and debug info')
    parser.add_argument('--dry-run', action='store_true', help='Show prompt without calling LLM')
    parser.add_argument('--prompt-file', help='Path to a custom prompt template file (overrides config.ini).')
    
    args = parser.parse_args()
    
    try:
        config = get_config()
        classifier = EmailClassifier(
            config=config,
            debug=args.debug, 
            dry_run=args.dry_run, 
            prompt_file=args.prompt_file
        )
        classifier.run(args.file)
    except (ImportError, RuntimeError, ValueError) as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
