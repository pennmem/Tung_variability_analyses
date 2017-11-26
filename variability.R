library(data.table)
library(dplyr)
data_path = 'ltpfr2_data.csv'
data_frame = fread(data_path)
data_frame[,'recallability'] = rep(0, dim(data_frame)[1])

complete_indices = which(complete.cases(data_frame))
data_frame = data_frame[complete_indices,]
subjects = unique(data_frame[,subject])



for (i in 1:length(subjects))
{
  subject = subjects[i]
  cat("subject: ", subject, '\n')
  indices = which(data_frame[,subject] == subject)
  data_frame_subject = data_frame[indices,]
  indices_not_subject = which(data_frame[,subject] != subject)
  data_frame_not_subject = data_frame[indices_not_subject,]
  
  unique_items = unique(data_frame_subject[,item])
  for(item in unique_items)
  {
    item_indices = which(data_frame_not_subject[,item] == item)
    data_frame_item = data_frame_not_subject[item_indices, ]
    data_frame_item_by_subject = group_by(data_frame_item, subject)
    avg_recall = summarize(data_frame_item_by_subject,  mean = mean(Recallability))[,2]
    avg_recall = sum(avg_recall)/dim(avg_recall)[1]
    
    item_mask = which(data_frame_subject[,item] == item)
    data_frame_subject[item_mask,recallability := avg_recall]
  }
  data_frame[indices, ] = data_frame_subject
  
}


