# helper functions
import pandas as pd
import numpy as np
import glob
import scipy
import re
import sys

rhino_root = ''

path = rhino_root + '/scratch/tphan/variability/'

subjects_data =glob.glob(rhino_root + '/scratch/tphan/variability/dataset_*.csv')
print(len(subjects_data))

data_frame = pd.DataFrame()
for i,subject_path in enumerate(subjects_data):
    print subject_path,i
    subject_data = pd.read_csv(subject_path)
    np.sum(np.sum(pd.isnull(subject_data)))
    n_sess = subject_data.shape[0]/576
    if n_sess == 23:
        data_frame = pd.concat([data_frame, subject_data])
        print subject_data.shape

data_frame.to_csv('/scratch/tphan/variability/variability/ltpfr2_data.csv')

ltpfr2_data_path = '/Volumes/RHINO/scratch/tphan/variability/variability/ltpfr2_data.csv'

data_frame = pd.read_csv(ltpfr2_data_path)