import sys
import os
import json

# Add src/python to path to allow importing project modules
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src/python')))

from config import get_config
from llm_client import get_llm_client

def main():
    """
    A simple integration test for the LLM client.
    It loads the configuration from config.ini, sends a test prompt to the
    configured LLM provider, and prints the response.
    """
    print("--- Running LLM Integration Test ---")
    try:
        config = get_config()
        llm_type = config.get('llm', 'type', 'N/A')
        print(f"Found config. Using LLM provider: {llm_type}")

        client = get_llm_client(config)
        
        prompt = "This is a test. Reply with only the word 'success'."
        print(f"Sending prompt: \"{prompt}\"")

        response = client.complete(prompt)
        
        print("\n--- LLM Response ---")
        print(response)
        print("--------------------")

        # Check for common error indicators
        try:
            # Try to parse as JSON to see if it's an error from our client
            response_json = json.loads(response)
            if 'error' in response_json:
                print("\nTest Result: FAILED (LLM client returned an error)")
                sys.exit(1)
        except json.JSONDecodeError:
            # Not a JSON error, which is good. Check for the word 'success'.
            pass

        if 'success' in response.lower():
            print("\nTest Result: PASSED")
        else:
            print("\nTest Result: FAILED (Response did not contain 'success')")
            sys.exit(1)

    except Exception as e:
        print(f"\nAn error occurred during the test: {e}")
        print("\nTest Result: FAILED")
        sys.exit(1)

if __name__ == "__main__":
    main()
