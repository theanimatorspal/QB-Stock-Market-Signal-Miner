import subprocess
import os

def run():
    os.chdir("R")
    subprocess.run(["Rscript", "model.R"])