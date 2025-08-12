import unittest
import tempfile
import os
from pathlib import Path
import sys

# Add src/python to path to allow importing config
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src/python')))
from config import MailConfig

class TestConfigValidation(unittest.TestCase):

    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.config_path = Path(self.temp_dir.name) / "config.ini"

    def tearDown(self):
        self.temp_dir.cleanup()

    def write_config(self, content):
        with open(self.config_path, "w") as f:
            f.write(content)

    def test_valid_cmd_config(self):
        """Test a valid configuration with llm.type = cmd."""
        self.write_config("""
[paths]
database = /tmp/test.db
[llm]
type = cmd
command = echo "test"
        """)
        try:
            MailConfig(config_path=str(self.config_path))
        except ValueError as e:
            self.fail(f"Validation failed unexpectedly: {e}")

    def test_valid_openai_config(self):
        """Test a valid configuration with llm.type = openai."""
        self.write_config("""
[paths]
database = /tmp/test.db
[llm]
type = openai
api_key = sk-12345
model = gpt-4
        """)
        try:
            MailConfig(config_path=str(self.config_path))
        except ValueError as e:
            self.fail(f"Validation failed unexpectedly: {e}")

    def test_invalid_llm_type(self):
        """Test that an invalid llm.type raises ValueError."""
        self.write_config("""
[paths]
database = /tmp/test.db
[llm]
type = invalid_type
        """)
        with self.assertRaisesRegex(ValueError, "type must be one of 'cmd', 'ollama', 'openai'"):
            MailConfig(config_path=str(self.config_path))

    def test_missing_llm_command_for_cmd_type(self):
        """Test that missing command for type=cmd raises ValueError."""
        self.write_config("""
[paths]
database = /tmp/test.db
[llm]
type = cmd
command = 
        """)
        with self.assertRaisesRegex(ValueError, "'command' is required when type is 'cmd'"):
            MailConfig(config_path=str(self.config_path))

    def test_missing_openai_key(self):
        """Test that missing api_key for type=openai raises ValueError."""
        self.write_config("""
[paths]
database = /tmp/test.db
[llm]
type = openai
model = gpt-4
api_key =
        """)
        with self.assertRaisesRegex(ValueError, "'api_key' is required when type is 'openai'"):
            MailConfig(config_path=str(self.config_path))

    def test_missing_database_path(self):
        """Test that a missing database path raises ValueError."""
        # We need to override the default to be empty
        self.write_config("""
[paths]
database = 
[llm]
type = cmd
command = echo "test"
        """)
        with self.assertRaisesRegex(ValueError, "'database' must be set"):
            MailConfig(config_path=str(self.config_path))

if __name__ == '__main__':
    unittest.main()
