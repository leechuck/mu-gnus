#!/usr/bin/env python3
"""
Convert email from stdin to org-mode format.
"""

import sys
import email
import argparse
from datetime import datetime
from email import policy
from email.utils import parsedate_to_datetime
import re
import html
import os

class EmailToOrg:
    def __init__(self, template_file=None, scheduled=False):
        self.template_file = template_file
        self.scheduled = scheduled
        self.template = None
        
        if template_file and os.path.exists(template_file):
            with open(template_file, 'r') as f:
                self.template = f.read()
    
    def read_email(self, source=None):
        """Read email from stdin or file."""
        if source:
            with open(source, 'rb') as f:
                msg = email.message_from_binary_file(f, policy=policy.default)
        else:
            msg = email.message_from_binary_file(sys.stdin.buffer, policy=policy.default)
        return msg
    
    def extract_body(self, msg):
        """Extract body from email, handling multipart messages."""
        body_parts = []
        
        if msg.is_multipart():
            for part in msg.walk():
                content_type = part.get_content_type()
                content_disposition = str(part.get("Content-Disposition", ""))
                
                # Skip attachments
                if "attachment" in content_disposition:
                    continue
                
                if content_type == "text/plain":
                    try:
                        body = part.get_content()
                        if body:
                            body_parts.append(body)
                    except Exception:
                        # Handle decoding errors
                        try:
                            body = part.get_payload(decode=True).decode('utf-8', errors='replace')
                            if body:
                                body_parts.append(body)
                        except Exception:
                            pass
                elif content_type == "text/html" and not body_parts:
                    # Only use HTML if no plain text available
                    try:
                        html_body = part.get_content()
                        # Simple HTML to text conversion
                        text_body = self.html_to_text(html_body)
                        if text_body:
                            body_parts.append(text_body)
                    except Exception:
                        try:
                            html_body = part.get_payload(decode=True).decode('utf-8', errors='replace')
                            text_body = self.html_to_text(html_body)
                            if text_body:
                                body_parts.append(text_body)
                        except Exception:
                            pass
        else:
            # Single part message
            content_type = msg.get_content_type()
            try:
                if content_type == "text/plain":
                    body = msg.get_content()
                    if body:
                        body_parts.append(body)
                elif content_type == "text/html":
                    html_body = msg.get_content()
                    text_body = self.html_to_text(html_body)
                    if text_body:
                        body_parts.append(text_body)
            except Exception:
                # Fallback for older email format
                try:
                    body = msg.get_payload(decode=True).decode('utf-8', errors='replace')
                    if body:
                        body_parts.append(body)
                except Exception:
                    body_parts.append("[Unable to extract email body]")
        
        return "\n\n".join(body_parts) if body_parts else "[Empty email body]"
    
    def html_to_text(self, html_content):
        """Simple HTML to text conversion."""
        # Remove HTML tags
        text = re.sub('<style[^<]*</style>', '', html_content, flags=re.IGNORECASE | re.DOTALL)
        text = re.sub('<script[^<]*</script>', '', text, flags=re.IGNORECASE | re.DOTALL)
        text = re.sub('<[^>]+>', '', text)
        
        # Decode HTML entities
        text = html.unescape(text)
        
        # Clean up whitespace
        lines = []
        for line in text.split('\n'):
            line = line.strip()
            if line:
                lines.append(line)
        
        return '\n'.join(lines)
    
    def format_date(self, date_str):
        """Format date string to org-mode format."""
        if not date_str:
            return datetime.now().strftime("%Y-%m-%d %a %H:%M")
        
        try:
            dt = parsedate_to_datetime(date_str)
            return dt.strftime("%Y-%m-%d %a %H:%M")
        except Exception:
            # Fallback to current date if parsing fails
            return datetime.now().strftime("%Y-%m-%d %a %H:%M")
    
    def escape_org_text(self, text):
        """Escape special characters for org-mode."""
        if not text:
            return ""
        # Escape asterisks at line start to prevent heading interpretation
        lines = text.split('\n')
        escaped_lines = []
        for line in lines:
            if line.strip().startswith('*'):
                line = ',' + line
            escaped_lines.append(line)
        return '\n'.join(escaped_lines)
    
    def convert_to_org(self, msg):
        """Convert email message to org-mode format."""
        # Extract headers
        subject = msg.get('Subject', 'No Subject')
        from_addr = msg.get('From', 'Unknown')
        message_id = msg.get('Message-ID', '')
        date_str = msg.get('Date', '')
        
        # Format date
        formatted_date = self.format_date(date_str)
        
        # Extract body
        body = self.extract_body(msg)
        
        if self.template:
            # Use custom template
            org_content = self.template
            org_content = org_content.replace('{{SUBJECT}}', subject)
            org_content = org_content.replace('{{FROM}}', from_addr)
            org_content = org_content.replace('{{MESSAGE_ID}}', message_id)
            org_content = org_content.replace('{{DATE}}', formatted_date)
            org_content = org_content.replace('{{BODY}}', body)
            
            # Add scheduled if requested
            if self.scheduled:
                today = datetime.now().strftime("<%Y-%m-%d %a>")
                org_content = org_content.replace('{{SCHEDULED}}', f"SCHEDULED: {today}")
            else:
                org_content = org_content.replace('{{SCHEDULED}}', '')
            
            return org_content
        else:
            # Default format
            org_lines = []
            
            # TODO heading with subject
            org_lines.append(f"* TODO {subject}")
            
            # Add SCHEDULED if requested
            if self.scheduled:
                today = datetime.now().strftime("<%Y-%m-%d %a>")
                org_lines.append(f"SCHEDULED: {today}")
            
            # Properties drawer
            org_lines.append(":PROPERTIES:")
            org_lines.append(f":MESSAGE_ID: {message_id}")
            org_lines.append(f":FROM: {from_addr}")
            org_lines.append(f":DATE: {formatted_date}")
            org_lines.append(":END:")
            
            # Empty line before body
            org_lines.append("")
            
            # Body as quote block
            org_lines.append("#+BEGIN_QUOTE")
            org_lines.append(self.escape_org_text(body))
            org_lines.append("#+END_QUOTE")
            
            return '\n'.join(org_lines)
    
    def run(self, source=None):
        """Main conversion process."""
        try:
            msg = self.read_email(source)
            org_content = self.convert_to_org(msg)
            print(org_content)
            return 0
        except Exception as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

def main():
    parser = argparse.ArgumentParser(description='Convert email to org-mode format')
    parser.add_argument('--template', type=str, help='Path to custom org template file')
    parser.add_argument('--scheduled', action='store_true', 
                       help='Add SCHEDULED: <today> to the org entry')
    
    args = parser.parse_args()
    
    converter = EmailToOrg(template_file=args.template, scheduled=args.scheduled)
    sys.exit(converter.run())

if __name__ == '__main__':
    main()
