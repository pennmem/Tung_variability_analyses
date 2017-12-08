# helper functions
import pandas as pd
import numpy as np
import glob
import scipy
import re
import sys
from ptsa.data.readers import EEGReader
import tables
rhino_root = ''
path = rhino_root + '/scratch/tphan/variability/dataset.csv'

data = pd.read_csv(path)