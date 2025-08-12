#!/usr/bin/env python3
"""
Configuration module for mail assistant.
Reads configuration from config.ini file.
"""

import os
import configparser
from pathlib import Path
from typing import Dict, Any, Optional, List

class MailConfig:
    """Configuration manager for mail assistant."""
    
    def __init__(self, config_path: Optional[str] = None):
        """
        Initialize configuration.
        
        Args:
            config_path: Path to config file. If None, searches standard locations.
        """
        self.config = configparser.ConfigParser()
        self.config_path = self._find_config_file(config_path)
        self._load_config()
        self._apply_defaults()
        self.validate()

    def validate(self) -> None:
        """
        Validate the configuration.
        Raises ValueError if configuration is invalid.
        """
        errors: List[str] = []
        
        # Validate LLM config
        llm_type = self.get('llm', 'type')
        if llm_type not in ['cmd', 'ollama', 'openai']:
            errors.append(f"[llm] type must be one of 'cmd', 'ollama', 'openai', but got '{llm_type}'")
        
        if llm_type == 'cmd':
            if not self.get('llm', 'command'):
                errors.append("[llm] 'command' is required when type is 'cmd'")
        elif llm_type == 'ollama':
            if not self.get('llm', 'api_url'):
                errors.append("[llm] 'api_url' is required when type is 'ollama'")
            if not self.get('llm', 'model'):
                errors.append("[llm] 'model' is required when type is 'ollama'")
        elif llm_type == 'openai':
            if not self.get('llm', 'api_key'):
                errors.append("[llm] 'api_key' is required when type is 'openai'")
            if not self.get('llm', 'model'):
                errors.append("[llm] 'model' is required when type is 'openai'")

        # Validate paths
        if not self.get('paths', 'database'):
            errors.append("[paths] 'database' must be set")

        if errors:
            raise ValueError("Configuration validation failed:\n- " + "\n- ".join(errors))
    
    def _find_config_file(self, config_path: Optional[str] = None) -> Optional[Path]:
        """
        Find configuration file in standard locations.
        
        Search order:
        1. Explicitly provided path
        2. MAIL_CONFIG_PATH environment variable
        3. ./config.ini (current directory)
        4. ~/.config/mail-assistant/config.ini
        5. /etc/mail-assistant/config.ini
        
        Returns:
            Path to config file if found, None otherwise.
        """
        if config_path:
            path = Path(config_path).expanduser()
            if path.exists():
                return path
        
        # Check environment variable
        env_path = os.environ.get('MAIL_CONFIG_PATH')
        if env_path:
            path = Path(env_path).expanduser()
            if path.exists():
                return path
        
        # Check standard locations
        search_paths = [
            Path('./config.ini'),
            Path('~/.config/mail-assistant/config.ini').expanduser(),
            Path('/etc/mail-assistant/config.ini')
        ]
        
        for path in search_paths:
            if path.exists():
                return path
        
        return None
    
    def _load_config(self):
        """Load configuration from file."""
        if self.config_path and self.config_path.exists():
            self.config.read(self.config_path)
    
    def _apply_defaults(self):
        """Apply default values for missing configuration."""
        defaults = {
            'paths': {
                'database': '~/.mail.db',
                'prompt_file': 'prompts/classify.txt',
                'org_template': ''
            },
            'llm': {
                'type': 'cmd',
                'command': 'echo \'{"category": "automated", "urgency": "low", "sender_type": "system"}\'',
                'model': 'gpt-3.5-turbo',
                'api_key': '',
                'api_url': 'http://localhost:11434'
            },
            'classification': {
                'categories': 'important, newsletter, social, automated, spam',
                'default_category': 'automated',
                'auto_needs_reply': 'true'
            },
            'processing': {
                'max_email_size': '10485760',
                'max_body_length': '1000'
            },
            'database': {
                'retention_days': '0',
                'auto_vacuum': 'false'
            }
        }
        
        for section, options in defaults.items():
            if not self.config.has_section(section):
                self.config.add_section(section)
            for option, value in options.items():
                if not self.config.has_option(section, option):
                    self.config.set(section, option, value)
    
    def get(self, section: str, option: str, fallback: Any = None) -> str:
        """
        Get configuration value.
        
        Args:
            section: Configuration section name.
            option: Option name within section.
            fallback: Default value if option not found.
        
        Returns:
            Configuration value as string.
        """
        return self.config.get(section, option, fallback=fallback)
    
    def getint(self, section: str, option: str, fallback: int = 0) -> int:
        """Get configuration value as integer."""
        return self.config.getint(section, option, fallback=fallback)
    
    def getboolean(self, section: str, option: str, fallback: bool = False) -> bool:
        """Get configuration value as boolean."""
        return self.config.getboolean(section, option, fallback=fallback)
    
    def getpath(self, section: str, option: str, fallback: str = '') -> Path:
        """
        Get configuration value as Path, expanding ~ and environment variables.
        
        Args:
            section: Configuration section name.
            option: Option name within section.
            fallback: Default value if option not found.
        
        Returns:
            Expanded Path object.
        """
        value = self.get(section, option, fallback=fallback)
        if not value:
            return Path()
        
        # Expand environment variables
        value = os.path.expandvars(value)
        # Expand ~ to home directory
        return Path(value).expanduser()
    
    def get_list(self, section: str, option: str, fallback: list = None) -> list:
        """
        Get configuration value as list (comma-separated).
        
        Args:
            section: Configuration section name.
            option: Option name within section.
            fallback: Default list if option not found.
        
        Returns:
            List of trimmed strings.
        """
        if fallback is None:
            fallback = []
        
        value = self.get(section, option, fallback='')
        if not value:
            return fallback
        
        return [item.strip() for item in value.split(',')]
    
    def get_db_path(self) -> Path:
        """Get database file path."""
        return self.getpath('paths', 'database', fallback='~/.mail.db')
    
    def get_prompt_file(self) -> Path:
        """Get prompt file path."""
        return self.getpath('paths', 'prompt_file', fallback='prompts/classify.txt')
    
    def get_org_template(self) -> Optional[Path]:
        """Get org template file path if configured."""
        path = self.getpath('paths', 'org_template')
        return path if path and path.exists() else None
    
    def get_llm_config(self) -> Dict[str, Any]:
        """Get LLM configuration as dictionary."""
        return {
            'type': self.get('llm', 'type', fallback='cmd'),
            'command': self.get('llm', 'command'),
            'model': self.get('llm', 'model'),
            'api_key': self.get('llm', 'api_key'),
            'api_url': self.get('llm', 'api_url')
        }
    
    def get_categories(self) -> list:
        """Get list of classification categories."""
        return self.get_list('classification', 'categories', 
                           fallback=['important', 'newsletter', 'social', 'automated', 'spam'])
    
    def get_default_category(self) -> str:
        """Get default classification category."""
        return self.get('classification', 'default_category', fallback='automated')
    
    def should_auto_mark_needs_reply(self) -> bool:
        """Check if important emails should be auto-marked as needing reply."""
        return self.getboolean('classification', 'auto_needs_reply', fallback=True)
    
    def get_max_email_size(self) -> int:
        """Get maximum email size to process (in bytes)."""
        return self.getint('processing', 'max_email_size', fallback=10485760)
    
    def get_max_body_length(self) -> int:
        """Get maximum body length for classification (in characters)."""
        return self.getint('processing', 'max_body_length', fallback=1000)
    
    def __str__(self) -> str:
        """String representation of configuration."""
        if self.config_path:
            return f"MailConfig(config_file={self.config_path})"
        else:
            return "MailConfig(using defaults)"


# Singleton instance
_config_instance = None

def get_config(config_path: Optional[str] = None) -> MailConfig:
    """
    Get configuration instance (singleton).
    
    Args:
        config_path: Path to config file (only used on first call).
    
    Returns:
        MailConfig instance.
    """
    global _config_instance
    if _config_instance is None:
        _config_instance = MailConfig(config_path)
    return _config_instance


if __name__ == '__main__':
    # Test/debug: print current configuration
    import sys
    
    config = get_config()
    print(f"Configuration loaded from: {config.config_path or 'defaults'}")
    print("\nCurrent configuration:")
    
    for section in config.config.sections():
        print(f"\n[{section}]")
        for option in config.config.options(section):
            value = config.get(section, option)
            # Mask sensitive values
            if 'key' in option.lower() or 'password' in option.lower():
                if value:
                    value = value[:4] + '***' if len(value) > 4 else '***'
            print(f"  {option} = {value}")
