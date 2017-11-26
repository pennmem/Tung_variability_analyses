# python code for processing LTPFR2 Data
from ptsa.data.TimeSeriesX import TimeSeriesX
from ptsa.data.common import xr
from ptsa.data.readers import BaseEventReader



import pandas as pd
import numpy as np
import glob
import scipy
import re
import sys

rhino_root = '/Volumes/RHINO'
subject_path = rhino_root + '/data/eeg/scalp/ltp/ltpFR2/behavioral/events/events_all_LTP287.mat'
# read data with BaseEventReader

events = BaseEventReader(filename=subject_path, common_root='data').read()
events = pd.DataFrame.from_records([e for e in events], columns=events.dtype.names)
word_events = events[events.type == 'WORD']
# word_events = wob rd_events.iloc[np.where(word_events.recalled > -998)[0]]
n_sessions = word_events.shape[0] / 576
subject = np.unique(events['subject'])[0]
subject_id = int(re.findall('\d+', subject)[0])
print n_sessions
n_sessions = len(np.unique(word_events.session))