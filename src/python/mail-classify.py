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
    def __init__(self, debug=False, dry_run=False, prompt_file=None):
        self.debug = debug
        self.dry_run = dry_run
        self.prompt_template = self._load_prompt_template(prompt_file)
        
        self.llm_client = None
        if not dry_run:
            if not HAS_LLM_CLIENT:
                raise ImportError("llm_client.py not found, which is required for classification.")
            try:
                self.llm_client = LLMClient(no_cache=False)
                if debug:
                    print(f"Using unified LLM client with backend: {self.llm_client.backend}", file=sys.stderr)
            except Exception as e:
                # Re-raise as a more informative error
                raise RuntimeError(f"Failed to initialize LLM client: {e}") from e

    def _load_prompt_template(self, prompt_file=None):
        """Load prompt template from file or use default."""
        if prompt_file and os.path.exists(prompt_file):
            with open(prompt_file, 'r') as f:
                return f.read()
        
        # Default prompt path relative to this script
        default_prompt_path = Path(__file__).parent.parent.parent / "prompts" / "classify.txt"
        if default_prompt_path.exists():
            return default_prompt_path.read_text()
        
        # Fallback hardcoded prompt if file is missing
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

Respond in JSON: {{"category": "...", "urgency": "...", "sender_type": "..."}}
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
        
        # Truncate body to first 1000 chars
        body = body[:1000] if body else ''
        
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
        if os.getenv("MAIL_LLM_TYPE") == "cmd":
            cmd = os.getenv("MAIL_LLM_CMD")
            if not cmd:
                return {"category": "automated", "urgency": "low", "sender_type": "system", "error": "MAIL_LLM_TYPE is 'cmd' but MAIL_LLM_CMD is not set"}
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
    parser.add_argument('--prompt-file', help='Path to a custom prompt template file.')
    
    args = parser.parse_args()
    
    try:
        classifier = EmailClassifier(
            debug=args.debug, 
            dry_run=args.dry_run, 
            prompt_file=args.prompt_file
        )
        classifier.run(args.file)
    except (ImportError, RuntimeError) as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
