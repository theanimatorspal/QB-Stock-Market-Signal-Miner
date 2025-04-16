import os
import subprocess

import rpy2.robjects as robj
from rpy2.robjects import r, pandas2ri


def run():
    pandas2ri.activate()
    os.chdir("R")
    subprocess.run(["Rscript", "app.R"])