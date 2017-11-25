# python code for processing LTPFR2 Data

from ptsa.data.TimeSeriesX import TimeSeriesX
from ptsa.data.common import xr
import pandas as pd
import numpy as np
import glob
import scipy
import re

# read in events
rhino_root = '/Volumes/RHINO'
from ptsa.data.readers import BaseEventReader
import tables

# grab all subjects
subjects_stat_data =glob.glob(rhino_root + '/data/eeg/scalp/ltp/ltpFR2/behavioral/data/stat_data_LTP*.mat')
subjects= glob.glob(rhino_root + '/data/eeg/scalp/ltp/ltpFR2/behavioral/events/events_all_*.mat')

subjects_full_list = []
subjects_index_list = []


i = 0
subject_path = subjects[i]
#for i,subject_path in enumerate(subjects):
try:
    events = BaseEventReader(filename=subject_path,common_root='data').read()
    events = pd.DataFrame.from_records([e for e in events],columns = events.dtype.names)
    word_events = events[events.type=='WORD']
    word_events = word_events.iloc[np.where(word_events.recalled > -998)[0]]
    n_sessions = word_events.shape[0]/576
    subject = np.unique(events['subject'])[0]
    subject_id = int(re.findall('\d+', subject)[0])
    # print n_sessions
    # n_sessions = len(np.unique(word_events.session))
    # print n_sessions
    # subjects_full_list.append(n_sessions)
    # subjects_index_list.append(i)
except: 
    print "error"

if n_sessions >= 23:
    
    path = rhino_root + '/data/eeg/scalp/ltp/ltpFR2/subj_and_sess_info.mat'
    datamat = scipy.io.loadmat(path)
    data_sess_info = datamat['sess_info']
    ltpfr_subject_ids = data_sess_info['subject'][0,0] # all ltpfr and ltpfr2 subjects

    indices = np.where(ltpfr_subject_ids == subject_id)[0]
    ltpfr_subject_sess = np.array(data_sess_info['session'][0,0][indices], dtype = 'int16')

    ltpfr_sleep = data_sess_info['sleep'][0,0][indices] # number of hours of sleep
    ltpfr_alertness = data_sess_info['alertness'][0,0][indices] # alertness
    ltpfr_start_time = data_sess_info['start_time'][0,0][indices]  # start_time
    ltpfr_day = data_sess_info['dayOfWeek'][0,0][indices]  # start_time


    event_session = word_events.session
    unique_sessions = np.unique(event_session)

    word_events['Alertness'] = -np.nan
    word_events['Sleep'] = -np.nan
    word_events['Time'] = -np.nan
    word_events['Day'] = -np.nan

    for i,sess in enumerate(unique_sessions):
        mask = event_session == sess
        index = np.where(ltpfr_subject_sess == sess)[0]
        word_events['Alertness'][mask]= ltpfr_alertness[index][0][0]
        word_events['Sleep'][mask]= ltpfr_sleep[index][0][0]
        word_events['Time'][mask]= ltpfr_start_time[index][0][0]
        word_events['Day'][mask]= ltpfr_day[index][0][0]

