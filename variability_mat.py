# python code for processing LTPFR2 Data

from ptsa.data.TimeSeriesX import TimeSeriesX
from ptsa.data.common import xr
import pandas as pd
import numpy as np
import glob
import scipy
import re
import sys
import scipy.io


args = sys.argv

print args

subject_index = int(args[1])


# read in

# read in events
rhino_root = '/Volumes/RHINO'
from ptsa.data.readers import BaseEventReader
import tables

# grab all subjects
subjects_stat_data =glob.glob(rhino_root + '/data/eeg/scalp/ltp/ltpFR2/behavioral/data/stat_data_LTP*.mat')
subjects= glob.glob(rhino_root + '/data/eeg/scalp/ltp/ltpFR2/behavioral/events/events_all_*.mat')


subjects_full_list = []
subjects_index_list = []


subject_path = subjects[subject_index]
print subject_path
# for i,subject_path in enumerate(subjects):
print 'index = ', subject_index
try:

    data = scipy.io.loadmat(subject_path, squeeze_me=True, struct_as_record=False)
    #print 'pass'
    events = data['events']
    events_type = np.array([event.type for event in events])
    word_events = events[events_type == 'WORD']
    session = np.array([event.session for event in word_events])
    n_sessions = len(session)/576

    subject = word_events[0].subject
    subject_id = int(re.findall('\d+', subject)[0])
    n_sessions = len(np.unique(session))

    print n_sessions

    if n_sessions == 24:

		word_events = word_events[session < 24]
		session = session[session < 24]


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


		session = session-1
		unique_sessions = np.unique(session)


		# create a data frame to store data
		data_frame = pd.DataFrame()

		recalled = np.array([event.recalled for event in word_events])
		recalled.astype('int')

		print 'number of nans', np.sum(pd.isnull(recalled))

		subject = [event.subject for event in word_events]

		print word_events.dtype

		list_num = [event.list for event in word_events]

		data_frame['subject'] = np.array(subject)
		data_frame['recalled'] = recalled
		data_frame['session'] = np.array(session)
		data_frame['list'] = np.array(list_num)



		data_frame['Alertness'] = -np.nan
		data_frame['Sleep'] = -np.nan
		data_frame['Time'] = -np.nan
		data_frame['Day'] = -np.nan
		data_frame['Recallability'] = -np.nan


		for i,sess in enumerate(unique_sessions):
		    print i, sess
		    mask = session == sess
		    index = np.where(ltpfr_subject_sess == sess)[0]
		    data_frame['Alertness'][mask]= ltpfr_alertness[index][0][0]
		    data_frame['Sleep'][mask]= ltpfr_sleep[index][0][0]
		    data_frame['Time'][mask]= ltpfr_start_time[index][0][0]
		    data_frame['Day'][mask]= ltpfr_day[index][0][0]



		# contruct recalliablity
		word_items = np.array([event.item for event in word_events])
		data_frame['item'] = word_items
		word_pool = np.unique(word_items)
		print len(word_pool)

		for i,word in enumerate(word_pool):
			#print word
			word_positions = (word_items == word)
			#print np.sum(word_positions)
			data_frame['Recallability'][word_positions] = np.mean(data_frame.recalled[word_positions])



		total_nans = np.sum(np.sum(pd.isnull(data_frame)))

		print total_nans
		if total_nans >= 0:

		    save_path = rhino_root + '/scratch/tphan/variability/dataset_'+ str(subject[0]) +'.csv'
		    print save_path
		    data_frame.to_csv(save_path, header = True)
		else:
		    print 'nans exist check subject data'
except:
    print "error"

