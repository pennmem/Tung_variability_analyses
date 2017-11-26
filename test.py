import glob
import matplotlib.pyplot as plt
import numpy as np
import scipy.io
#import statistics
import math


#FINDING PROBABILITY OF RECALL FOR EACH WORD IN LTPFR2


def get_ltpFR2_precall(files_ltpFR2):

    """This function gets the recall probability of each word for each participant
    dimenstions should be N participants * 576 words """

    # determining whether each word is recalled or not when it is presented to the participant
    # dimenstions should be (576 words * times word was presented (23) for each participant

    def count_recalls(rec_mat, pres_mat):
        word_id_recalled_or_not_this_part = []
        # print("rec mat", rec_mat)
        for word_id in np.unique(pres_mat):
            #print("Word ID:", word_id)
            word_id_recalled_or_not_this_part.append(rec_mat[np.where((pres_mat) == word_id)])
        #print("Shape Word id this part", np.shape(word_id_recalled_or_not_this_part))
        #print(word_id_recalled_or_not_this_part)
        return word_id_recalled_or_not_this_part

    all_participants_all_recalled = []

    # Do this for every participant
    for f in files_ltpFR2:
        # Read in data
        test_mat_file = scipy.io.loadmat(f, squeeze_me=True, struct_as_record=False)
        if isinstance(test_mat_file['data'], np.ndarray):
            #print('Skipping...')
            continue
        session_mat = test_mat_file['data'].session
        # print(len(session_mat))
        # print(np.bincount(session_mat))
        if len(session_mat) < 576:
            print('Skipping because participant did not finish...')
            continue
        else:
            print(f)
            pres_mat = test_mat_file['data'].pres_itemnos.astype('int16')
            pres_mat = pres_mat[np.where(session_mat != 24)]
            rec_mat = test_mat_file['data'].pres.recalled
            # rec_mat = rec_mat[np.where(session_mat != 24)]
            # print("Legth of pres_mat", (pres_mat).shape)
            # print("Length of rec_mat", (rec_mat).shape)

        # print("Pres mat", pres_mat)
        # print("Rec mat", rec_mat)
        #print(rec_mat.shape)

        # For each list of this participant's data
        this_participant_probs = []

        word_id_recalled_this_part = count_recalls(rec_mat, pres_mat)

        # Append this partic p_rec to everyone else
        all_participants_all_recalled.append(np.nanmean(word_id_recalled_this_part, axis = 1))

    #Get the average of all participants
    return all_participants_all_recalled

if __name__ == "__main__":
    files_ltpFR2 = glob.glob('/Volumes/RHINO/data/eeg/scalp/ltp/ltpFR2/behavioral/data/stat_data_LTP*.mat')
    all_participants_all_recalled = get_ltpFR2_precall([files_ltpFR2[52]])
    print(all_participants_all_recalled)
    print("Shape of all participants all recalled", np.shape(all_participants_all_recalled))

    y = []
    for i in range(len(all_participants_all_recalled[0])):
        y.append(np.array(all_participants_all_recalled)[:, i])
    print(np.array(y).shape)

    y = np.array(y)
    means = np.mean(y, axis=1)
    ind = np.argsort(means)
    y_ascending = y[ind]

    for i in range(len(all_participants_all_recalled[0])):
        x = [i] * (len(all_participants_all_recalled))
        y = y_ascending[i]
        plt.scatter(x, y, color='gray', s=2)
    plt.scatter((range(576)), np.sort(means), color='black', s=5)
    plt.xlabel("Word Number", size=13)
    plt.ylabel("Recall Probability", size=13)
    plt.savefig("Prec_words_ltpFR2.pdf")

    plt.show()




data = scipy.io.loadmat(subject_path, squeeze_me=True, struct_as_record=False)
events = data['events']

events_type = np.array([event.type for event in events])
word_events = events[events_type == 'WORD']
session = np.array([event.session for event in word_events])