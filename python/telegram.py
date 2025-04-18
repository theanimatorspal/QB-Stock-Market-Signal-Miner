import requests
import os

class Notify:
    def __init__(self, keys_path="../secrets/keys.txt"):
        self.bot_token = None
        self.chat_id = None
        self._load_keys(keys_path)

    def _load_keys(self, path):
        if not os.path.exists(path):
            raise FileNotFoundError(f"Keys file not found at {path}")
        
        with open(path, "r") as f:
            lines = f.read().splitlines()
            if len(lines) < 2:
                raise ValueError("Keys file must contain BOT_TOKEN and CHAT_ID on separate lines.")
            self.bot_token = lines[0].strip()
            self.chat_id = lines[1].strip()

    def send(self, message):
        url = f"https://api.telegram.org/bot{self.bot_token}/sendMessage"
        payload = {
            "chat_id": self.chat_id,
            "text": message,
            "parse_mode": "Markdown"
        }
        response = requests.post(url, data=payload)
        if not response.ok:
            raise Exception(f"Failed to send message: {response.text}")
