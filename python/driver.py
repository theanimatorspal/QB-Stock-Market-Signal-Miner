import os
os.environ["R_HOME"] = "C:\\Program Files\\R\\R-4.4.3"

import rpy2.robjects as robj
from rpy2.robjects import r, pandas2ri


def run():
    pandas2ri.activate()
    os.chdir("R")
    r.source("app.R")