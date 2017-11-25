# python code for processing LTPFR2 Data

from ptsa.data.TimeSeriesX import TimeSeriesX
from ptsa.data.common import xr
import pandas as pd
import numpy as np
import glob
import scipy


# read in events
rhino_root = '/Volumes/RHINO'
from ptsa.data.readers import BaseEventReader
import tables



# grab all subjects
subjects_stat_data =glob.glob(rhino_root + '/data/eeg/scalp/ltp/ltpFR2/behavioral/data/stat_data_LTP*.mat')
subjects_events = glob.glob(rhino_root + '/data/eeg/scalp/ltp/ltpFR2/behavioral/events/events_all_*.mat')

subjects_full_list = []

events = BaseEventReader(filename=subjects_events[30],common_root='data').read()
events = pd.DataFrame.from_records([e for e in events],columns = events.dtype.names)
word_events = events[events.type=='WORD']
n_sessions = len(word_events)/576
print n_sessions
n_sessions = np.unique(word_events.session)
print n_sessions


if (n_sessions == 23):
    subjects_full_list.append(subjects_events[1])




path = rhino_root + '/data/eeg/scalp/ltp/ltpFR2/subj_and_sess_info.mat'
datamat = scipy.io.loadmat(path)
data_sess_info = datamat['sess_info']
ltpfr_subject_ids = data_sess_info['subject'][0,0] # all ltpfr and ltpfr2 subjects
ltpfr_sleep = data_sess_info['sleep'][0,0]  # number of hours of sleep
ltpfr_alertness = data_sess_info['alertness'][0,0]  # alertness
ltpfr_start_time = data_sess_info['start_time'][0,0]  # start_time




