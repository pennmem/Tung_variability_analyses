# python code for processing LTPFR2 Data

from ptsa.data.readers import EEGReader
import tables
import sys
import matplotlib.pyplot as plt


from ptsa.data.TimeSeriesX import TimeSeriesX
from ptsa.data.common import xr
from ptsa.data.readers import BaseEventReader
from ptsa.data.filters import MorletWaveletFilterCpp
from ptsa.data.filters import ButterworthFilter


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


# read eegs
events = BaseEventReader(filename=rhino_root+'/data/eeg/scalp/ltp/ltpFR2/behavioral/events/events_all_LTP377.mat',
                         use_reref_eeg=True,common_root='data').read()
words = events[events.type=='WORD']

# select all available channels
eegfile = words[0].eegfile
eegfile_reref = str.split(str(eegfile),'/')
day = eegfile_reref[-1]
eegfile_reref = '/'.join(eegfile_reref[0:-1])
channels = glob.glob(eegfile_reref + '/' +  day + "*.[0-9]*")
channels = np.array([str.split(x, '.')[-1] for x in channels])
#channels = np.array(['{:03}'.format(x) for x in range(0,132)])

eeg = EEGReader(events=words,channels=channels,start_time=0.3,end_time=1.6).read()

b_filter = ButterworthFilter(time_series = eeg, order = 4, freq_range = [58,62])
eeg_filtered = b_filter.filter()
eeg_buffered = eeg_filtered.add_mirror_buffer(1.3)
eeg_buffered.data = np.ascontiguousarray(eeg_buffered.data)


import time
pt = time.time()
freqs = np.logspace(np.log10(3), np.log10(180), 50)
pow_ev,_ = MorletWaveletFilterCpp(time_series= eeg_buffered, freqs = freqs[0:1], output = 'power', cpus = 20, verbose = False).filter()
run_time = time.time() - pt
