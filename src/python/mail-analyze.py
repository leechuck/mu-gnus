#!/usr/bin/env python3
"""
mail-analyze.py - Email analysis tool with LLM integration
"""

import sys
import json
import argparse
from pathlib import Path
from typing import Optional, Dict, Any
import email
from email import policy

# Add src/python to path for imports
sys.path.insert(0, str(Path(__file__).parent))

from config import get_config
from llm_client import get_llm_client


class MailAnalyzer:
    """Email analyzer with various subcommands."""
    
    def __init__(self, config=None):
        """Initialize with configuration."""
        self.config = config or get_config()
        self.llm_client = get_llm_client(self.config)
    
    def read_email_from_stdin(self) -> str:
        """Read email content from stdin."""
        return sys.stdin.read()
    
    def extract_actions(self, email_text: str) -> Dict[str, Any]:
        """Extract action items from email using LLM."""
        
        # Parse email to get headers
        try:
            msg = email.message_from_string(email_text, policy=policy.default)
            subject = msg.get('Subject', '')
            from_addr = msg.get('From', '')
            date = msg.get('Date', '')
        except:
            # If parsing fails, use empty values
            subject = ''
            from_addr = ''
            date = ''
        
        prompt = f"""Extract action items from this email.

An action item is something the sender wants YOU to:
- Answer (question)
- Do (task)
- Decide (choice)
- Acknowledge (confirm receipt)

Email:
{email_text}

Return JSON:
{{
  "subject": "email subject",
  "from": "sender email",
  "date": "date string",
  "needs_reply": true/false,
  "actions": [
    "Answer: Will you attend the meeting?",
    "Task: Review the attached document",
    "Decide: Choose option A or B"
  ]
}}"""
        
        try:
            response = self.llm_client.complete(prompt, json_output=True)
            result = json.loads(response)
            
            # Ensure all required fields exist
            if 'subject' not in result:
                result['subject'] = subject
            if 'from' not in result:
                result['from'] = from_addr
            if 'date' not in result:
                result['date'] = date
            if 'needs_reply' not in result:
                result['needs_reply'] = False
            if 'actions' not in result:
                result['actions'] = []
                
            return result
        except Exception as e:
            # Return a default structure on error
            return {
                "subject": subject,
                "from": from_addr,
                "date": date,
                "needs_reply": False,
                "actions": [],
                "error": str(e)
            }
    
    def get_config_value(self, key: str) -> str:
        """Get a configuration value."""
        # Try to get from different sections
        for section in ['paths', 'llm', 'settings']:
            try:
                value = self.config.get(section, key)
                if value:
                    return value
            except:
                pass
        
        # Try special getters
        if key == 'org_file':
            try:
                org_template = self.config.get_org_template()
                if org_template:
                    return str(org_template)
            except:
                pass
        
        return ""


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description='Email analysis tool')
    parser.add_argument('command', choices=['extract-actions', '--extract-actions', 
                                           'get-config', '--get-config'],
                       help='Command to execute')
    parser.add_argument('config_key', nargs='?', help='Config key for get-config command')
    parser.add_argument('--config', help='Path to config file')
    
    args = parser.parse_args()
    
    # Handle both with and without dashes
    command = args.command.lstrip('-')
    
    # Initialize analyzer
    config = get_config(args.config)
    analyzer = MailAnalyzer(config)
    
    if command == 'extract-actions':
        # Read email from stdin
        email_text = analyzer.read_email_from_stdin()
        
        # Extract actions
        result = analyzer.extract_actions(email_text)
        
        # Output only JSON
        print(json.dumps(result, indent=2))
        
    elif command == 'get-config':
        if not args.config_key:
            print("", end="")
        else:
            value = analyzer.get_config_value(args.config_key)
            print(value, end="")
    
    else:
        parser.print_help()
        sys.exit(1)


if __name__ == '__main__':
    main()
