import os
import requests

url = "https://raw.githubusercontent.com/AnamolZ/retainAI/refs/heads/main/services/webscrapper/nepseScrapper.py"
local_path = "python/nepseScrapper.py"
try:
    response = requests.get(url)
    response.raise_for_status()
    os.makedirs(os.path.dirname(local_path), exist_ok=True)
    with open(local_path, "w", encoding="utf-8") as f:
        f.write(response.text)

    print(f"Script saved to: {local_path}")
except Exception as e:
    print(f"Error fetching or saving the script: {e}")


os.environ["R_HOME"] = "C:\\Program Files\\R\\R-4.4.3"

import subprocess
import rpy2.robjects as robj
from rpy2.robjects import r, pandas2ri

def run():
    pandas2ri.activate()
    os.chdir("R")
    subprocess.run(["Rscript", "app.R"])
    #subprocess.run(["Rscript", "scraper.R"])