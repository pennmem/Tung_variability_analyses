# python code for processing LTPFR2 Data

from ptsa.data.TimeSeriesX import TimeSeriesX
from ptsa.data.common import xr
import pandas as pd
import numpy as np
import glob
import scipy
import re
import sys


# args = sys.argv
#
# print args


# read in events
rhino_root = '/Volumes/RHINO'
from ptsa.data.readers import BaseEventReader
import tables

# grab all subjects
subjects_stat_data =glob.glob(rhino_root + '/data/eeg/scalp/ltp/ltpFR2/behavioral/data/stat_data_LTP*.mat')
subjects= glob.glob(rhino_root + '/data/eeg/scalp/ltp/ltpFR2/behavioral/events/events_all_*.mat')

subjects_full_list = []
subjects_index_list = []


i = 39

subject_path = subjects[i]
print subject_path

subject_path = "/Volumes/RHINO/data/eeg/scalp/ltp/ltpFR2/behavioral/events/events_all_LTP287.mat"
# for i,subject_path in enumerate(subjects):
print 'index = ', i
try:

    #data = scipy.io.loadmat(subject_path, squeeze_me=True, struct_as_record=False)

    events = BaseEventReader(filename=subject_path,common_root='data').read()
    events = pd.DataFrame.from_records([e for e in events],columns = events.dtype.names)
    word_events = events[events.type=='WORD']
    #word_events = wob rd_events.iloc[np.where(word_events.recalled > -998)[0]]
    n_sessions = word_events.shape[0]/576
    subject = np.unique(events['subject'])[0]
    subject_id = int(re.findall('\d+', subject)[0])
    print n_sessions
    n_sessions = len(np.unique(word_events.session))
    # print n_sessions
    # subjects_full_list.append(n_sessions)
    # subjects_index_list.append(i)
except:
    print "error"



if n_sessions == 24:
    word_events = word_events[word_events.session < 24]

# get session info
    path = rhino_root + '/data/eeg/scalp/ltp/ltpFR2/subj_and_sess_info.mat'
    datamat = scipy.io.loadmat(path)
    data_sess_info = datamat['sess_info']
    ltpfr_subject_ids = data_sess_info['subject'][0,0]# all ltpfr and ltpfr2 subjects
    indices = np.where(ltpfr_subject_ids == subject_id)[0]
    ltpfr_subject_sess = (np.array(data_sess_info['session'][0,0][indices]))
    ltpfr_subject_sess.astype('int')

    ltpfr_sleep = data_sess_info['sleep'][0,0][indices] # number of hours of sleep
    ltpfr_alertness = data_sess_info['alertness'][0,0][indices] # alertness
    ltpfr_start_time = data_sess_info['start_time'][0,0][indices]  # start_time
    ltpfr_day = data_sess_info['dayOfWeek'][0,0][indices]  # start_time


    event_session = word_events.session-1
    unique_sessions = np.unique(event_session)

    word_events['Alertness'] = -np.nan
    word_events['Sleep'] = -np.nan
    word_events['Time'] = -np.nan
    word_events['Day'] = -np.nan
    word_events['Recallability'] = -np.nan


    for i,sess in enumerate(unique_sessions):
        print i, sess
        mask = event_session == sess
        index = np.where(ltpfr_subject_sess == sess)[0]
        word_events['Alertness'][mask]= ltpfr_alertness[index][0][0]
        word_events['Sleep'][mask]= ltpfr_sleep[index][0][0]
        word_events['Time'][mask]= ltpfr_start_time[index][0][0]
        word_events['Day'][mask]= ltpfr_day[index][0][0]


    # contruct recalliablity
    word_items = word_events.item
    word_pool = np.unique(word_items)

    for i,word in enumerate(word_pool):
        word_positions = (word_items == word)
        word_events['Recallability'][word_positions] = np.mean(word_events.recalled[word_positions])



    total_nans = np.sum(np.sum(pd.isnull(word_events)))

    if total_nans == 0:

        save_path = rhino_root + '/scratch/tphan/variability/dataset.csv'
        word_events.to_csv(save_path, mode = 'a', header = False)
    else:
        print 'nans exist check subject data'
    #
