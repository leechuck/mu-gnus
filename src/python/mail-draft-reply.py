#!/usr/bin/env python3
"""
mail-draft-reply.py - Generate email reply drafts using LLM
"""

import sys
import argparse
from pathlib import Path

# Add src/python to path for imports
sys.path.insert(0, str(Path(__file__).parent))

from config import get_config
from llm_client import get_llm_client


def read_file(filepath: str) -> str:
    """Read content from a file."""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            return f.read()
    except Exception as e:
        print(f"Error reading file {filepath}: {e}", file=sys.stderr)
        sys.exit(1)


def draft_reply(original_email: str, user_response: str, config=None) -> str:
    """Generate a reply draft using LLM."""
    
    # Initialize LLM client
    if config is None:
        config = get_config()
    llm_client = get_llm_client(config)
    
    # Create prompt
    prompt = f"""Draft a reply-all email based on my responses.

Original email:
{original_email}

My responses/decisions:
{user_response}

Write a professional reply that:
- Addresses each point from my responses
- Uses appropriate greeting and signature
- Maintains the same tone as the original
- Is concise and clear

Output only the reply body text, no subject line."""
    
    try:
        # Get LLM response
        reply_draft = llm_client.complete(prompt)
        return reply_draft.strip()
    except Exception as e:
        print(f"Error generating reply: {e}", file=sys.stderr)
        sys.exit(1)


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description='Generate email reply drafts using LLM')
    parser.add_argument('--original', required=True, 
                       help='Path to original email file')
    parser.add_argument('--response', required=True,
                       help='Path to user response text file')
    parser.add_argument('--config', help='Path to config file')
    
    args = parser.parse_args()
    
    # Read input files
    original_email = read_file(args.original)
    user_response = read_file(args.response)
    
    # Get config if specified
    config = None
    if args.config:
        config = get_config(args.config)
    
    # Generate reply draft
    reply_draft = draft_reply(original_email, user_response, config)
    
    # Output the draft
    print(reply_draft)


if __name__ == '__main__':
    main()
