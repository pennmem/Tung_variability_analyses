library(data.table)
library(dplyr)
library(lme4)

logit = function(x)
{
  x[which(x==0)] = 0.001
  x[which(x ==1)] = 0.999 
  
  return(log(x/(1-x)))
}




data_path = 'ltpfr2_data_processed.csv'
ltpfr2 = fread(data_path)

ltpfr2 = as.data.frame(ltpfr2)
indices = which((ltpfr2['Day'] != 'Sat') & (ltpfr2['Day'] != 'Sun') )

ltpfr2 = ltpfr2[indices,]
# collapse data 
n_total_lists = dim(ltpfr2)[1]/24

ltpfr2_prob =data.frame()
for(j in 1:n_total_lists)
{
  if(j%%10000 ==0){ cat(j, "\n")}
  select_range = ((j-1)*24+1):(j*24)
  temp = ltpfr2[select_range,]
  prob = mean(temp[['recalled']])
  avg_recall = mean(temp[['recallability']])
  
  ltpfr2_prob = rbind(ltpfr2_prob, ltpfr2[(j-1)*24+1,])
  ltpfr2_prob[j,'recalled'] = prob
  ltpfr2_prob[j,'recallability'] = avg_recall
}

#write.table(ltpfr2_prob, 'ltpfr2_prob.csv', row.names = F)
# correlation table 
intersession = cor(ltpfr2_prob[c('session', 'Sleep', 'Time', 'Alertness' )])
xtable(intersession)

interlist = cor(ltpfr2_prob[c('session', 'Block', 'list', 'recallability')])
xtable(interlist)



ltpfr2_prob = fread('ltpfr2_prob.csv')
ltpfr2_prob = as.data.frame(ltpfr2_prob)
ltpfr2_prob['Sleep'] = ltpfr2_prob['Sleep']
ltpfr2_prob['Alertness'] = ltpfr2_prob['Alertness'] + rnorm(length(ltpfr2_prob[['Alertness']]),0,0.05)
ltpfr2_prob['Sess'] = ltpfr2_prob['session'] 
ltpfr2_prob['session'] = ltpfr2_prob['session'] + 1

#ltpfr2_prob['list'] = factor(ltpfr2_prob[['list']])
ltpfr2_prob['recalled'] = logit(ltpfr2_prob[['recalled']])
ltpfr2_prob['Time'] = scale(assign_time(ltpfr2_prob[['Time']]))
ltpfr2_prob['Block'] = scale(assign_block(ltpfr2_prob[['list']]))
ltpfr2_prob['list'] = assign_list(ltpfr2_prob[['list']])
ltpfr2_prob['list_squared'] = ltpfr2_prob[['list']]^2
ltpfr2_prob['recallability'] = scale(ltpfr2_prob['recallability'])

model_session = lmer(recalled ~ (1 + Time + Sleep + Alertness + session | subject) + 1 + Time + Sleep + Alertness + session , ltpfr2_prob)

save(model_session, file = "model_session.rda")

model_list = lmer(recalled ~ (1 + session + Block + list + list_squared +  recallability | subject) + 1 + session + Block + list + list_squared + recallability, ltpfr2_prob)



#save(model_session, file = 'model_session.rda')
#save(model_list, file = 'model_list.rda')
load('model_session.rda')
# get models 
#library(lmerTest)
summary(model_session)
coefs =coef(model_list)$subject
names(coefs)[9] = 'Session'
names(coefs)[2:5] = c("Mon", "Thu", "Tue", "Wed")

plot_model(coefs[c('Alertness', 'Session', 'Sleep', 'Time')], 10, 10, 'intersession_vars.pdf')
plot_model(coefs[c('Mon', 'Thu', 'Tue', 'Wed')], 10, 10, 'intersession_var_Day.pdf')

coeffs = as.data.frame(summary(model_session)$coefficients)
coeffs['p'] = 2*(1 - pnorm(abs(coeffs[,3])))

xtable(coeffs[,1:2], digits = 3)
 


residuals = resid(model_session)  # get residuals from each model 
subjects = unique(ltpfr2_prob[['subject']])
r_vec = length(subjects)
list_result = list()

intersession_coefs = data.frame()
intersession_p = data.frame()
for(i in 1:length(subjects))
{
  subject = subjects[i]
  indices = which((ltpfr2_prob[['subject']] == subject))
  subject_data = ltpfr2_prob[indices,]
  y = subject_data[['recalled']]
  SST = sum((y - mean(y))^2)
  residuals_subject = residuals[indices]
  SSE = sum(residuals[indices]^2)
  r = 1 - SSE/SST
  r_vec[i] = r 
  
  #predicted =inv.logit(residuals_subject + mean(y))
  predicted =inv.logit(y)
  subject_data[['predicted']] = predicted
  
  subject_data_group_by_Sess = group_by(subject_data, Sess)
  recall_prob = summarise( subject_data_group_by_Sess, recall_prob = mean(predicted))
  recall_prob = as.data.frame(recall_prob)
  list_result[i] = list(recall = recall_prob, r = r)
  
  regression_model = lm(recalled ~ Time + Sleep + Alertness + session, subject_data)
  
  outcome = lm.beta(regression_model)
  
  #intersession_coefs = rbind(intersession_coefs, summary(regression_model)$coefficients[,'Estimate'])
  intersession_coefs = rbind(intersession_coefs,lm.beta(regression_model))
  intersession_p = rbind(intersession_p, summary(regression_model)$coefficients[,4])
}

library(QuantPsyc)



names(intersession_coefs) = names(outcome)
names(intersession_p) = names(summary(regression_model)$coefficients[,4])

var_list = c('Session','Sleep', 'Alertness', 'Time')
names(intersession_coefs)[4] = 'Session'
names(intersession_p)[5] = 'Session'
d = intersession_coefs[var_list]
d_p = intersession_p[var_list]

plot_model(intersession_coefs[var_list], intersession_p[var_list], plot_title = 'interession_vars.pdf')





d = intersession_coefs[c('Session','Sleep', 'Alertness', 'Time')]
plot_model(d, plot_title = 'intersesion_vars.pdf')




means = sapply(list_result, function(x){mean(x$recall)})
sort_result = sort(means, index.return = TRUE)
ix = sort_result$ix

session_frame = data.frame()
for(i in 1:length(ix))
{
  temp = data.frame()
  
  recall_prob = list_result[[ix[i]]]$recall
  temp = data.frame(recall = recall_prob , number = rep(i,length(recall_prob)))
  session_frame = rbind(session_frame, temp)
}

p = ggplot(session_frame, aes(x = number, y = recall))  
p = p + geom_point(size = 0.7) + theme_bw() + ylim(c(0,1))
#p = p + geom_hline(yintercept = 0, linetype = 'dashed')

p = p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p = p + theme(text = element_text(size=15),
              axis.text.x = element_text(angle=0, hjust=1))
p = p + xlab('Subject Number') + ylab("Recall Probability") 

ggsave('prob_variablity_session.pdf', plot = p, device = NULL, path = NULL,
       scale = 1, dpi = 500, width = 10, height = 10, units = "cm")


r_vec_sort = r_vec[ix]
r_frame = data.frame(r = r_vec_sort, number = 1:length(r_vec_sort))
r_frame$r = 100*r_frame$r

p = ggplot(r_frame, aes(x = number, y = r))  
p = p + geom_point(size = 0.7) + theme_bw() + ylim(c(0,30))
p = p + geom_hline(yintercept = mean(r_frame$r), linetype = 'dashed')

p = p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p = p + theme(text = element_text(size=15),
              axis.text.x = element_text(angle=0, hjust=1)) 
p = p + xlab('Subject Number') + ylab("Percent Reduction")

ggsave('percent_reduction_session.pdf', plot = p, device = NULL, path = NULL,
       scale = 1, dpi = 500, width = 10, height = 10, units = "cm")




#load("model_list.rda")
plot(fitted(model_list), residuals(model_list))
coefs =coef(model_list)$subject
names(coefs)[10] = 'Session'
names(coefs)[11] = 'Recallability'

plot_model(coefs[c('Block', 'Recallability', 'Session')], 10, 10, 'interlists_vars.pdf')




ltpfr2_prob = fread('ltpfr2_prob.csv')
ltpfr2_prob = as.data.frame(ltpfr2_prob)

ltpfr2_prob['Sleep'] = scale(ltpfr2_prob['Sleep'])
ltpfr2_prob['Alertness'] = ltpfr2_prob['Alertness'] + rnorm(length(ltpfr2_prob[['Alertness']]),0,0.05)
ltpfr2_prob['Sess'] = ltpfr2_prob['session'] 

ltpfr2_prob['session'] = ltpfr2_prob['session'] + 1

#ltpfr2_prob['list'] = factor(ltpfr2_prob[['list']])
ltpfr2_prob['recalled'] = logit(ltpfr2_prob[['recalled']])
ltpfr2_prob['Time'] = scale(assign_time(ltpfr2_prob[['Time']]))
ltpfr2_prob['Block'] = scale(assign_block(ltpfr2_prob[['list']]))
ltpfr2_prob['List'] = as.integer(ltpfr2_prob[['list']])



ltpfr2_prob['list'] = as.integer(assign_list(ltpfr2_prob[['list']]))
ltpfr2_prob['List_squared'] = as.integer(ltpfr2_prob[['list']])^2
ltpfr2_prob['recallability'] = scale(ltpfr2_prob['recallability'])



residuals = resid(model_list)  # get residuals from each model 
subjects = unique(ltpfr2_prob[['subject']])
r_vec = length(subjects)
list_result = list()
auto_frame = data.frame()


interlist_coefs = data.frame()
interlist_p = data.frame()



for(i in 1:length(subjects))
{
  subject = subjects[i]
  indices = which((ltpfr2_prob[['subject']] == subject))
  subject_data = ltpfr2_prob[indices,]

  y = subject_data[['recalled']]
  SST = sum((y - mean(y))^2)
  residuals_subject = residuals[indices]
  SSE = sum(residuals[indices]^2)
  r = 1 - SSE/SST
  r_vec[i] = r 
  
  predicted =inv.logit(residuals_subject + mean(y))
  #predicted = inv.logit(y)
  
  subject_data[['predicted']] = predicted
  
  
  unique_sessions = unique(subject_data[['Sess']])
  
  n_sess = 0
  avg_recall = rep(0,24)
  for(sess in unique_sessions)
  {
    index_sess = which(subject_data[['Sess']] == sess)
    if(length(index_sess) == 24)
    {
      n_sess = n_sess +  1
      avg_recall = avg_recall + sort(predicted[index_sess])
    }else
    {
      cat("short list: ", length(index_sess), "\n")
    }
  }
  avg_recall = avg_recall/n_sess
  
  
  subject_data_group_by_Sess = group_by(subject_data, List)
  
  #recall_prob = summarise( subject_data_group_by_Sess, recall_prob = avg_recall)
  recall_prob = avg_recall
  list_result[[i]] = list(recall = recall_prob, r = r)
  
  indices_auto = which((2 <= subject_data[["list"]]) & (subject_data[["list"]] <=8) )
  indices_lagged = indices_auto-1
  
  auto_temp = data.frame(resid = residuals_subject[indices_auto], lagged = residuals_subject[indices_lagged], prior_recall = subject_data[indices_lagged,'recalled'], subject = subject_data[['subject']][1])
  
  auto_frame = rbind(auto_frame, auto_temp)
  regression_model = lm(recalled ~ session + Block + list + List_squared + recallability, subject_data)
  #interlist_coefs = rbind(interlist_coefs, summary(regression_model)$coefficients[,'Estimate'])
  
  outcome = lm.beta(regression_model)
  
  #intersession_coefs = rbind(intersession_coefs, summary(regression_model)$coefficients[,'Estimate'])
  interlist_coefs = rbind(interlist_coefs,lm.beta(regression_model))
  interlist_p = rbind(interlist_p, summary(regression_model)$coefficients[,4])
}


names(interlist_coefs) = names(outcome)
names(interlist_p) = names(summary(regression_model)$coefficients[,4])

var_list = c('Session','Block', 'List', 'Recallability')
names(interlist_coefs)[1] = 'Session'
names(interlist_coefs)[3] = 'List'
names(interlist_coefs)[5] = 'Recallability'
names(interlist_p)[2] = 'Session'
names(interlist_p)[6] = 'Recallability'
names(interlist_p)[4] = 'List'

d = intersession_coefs[var_list]
d_p = intersession_p[var_list]

plot_model(interlist_coefs[var_list], interlist_p[var_list], plot_title = 'interlist_vars.pdf')






auto_frame['subject'] = factor(auto_frame[['subject']])
# autocorrelation analysis 
#model_resid = lmer(resid ~ (1 + lagged + prior_recall | subject) +  1+ lagged + prior_recall , auto_frame)

coefs = as.data.frame(summary(model_resid)$coefficients)
coefs['p'] = 2*(1 - pnorm(abs(coefs[,3])))



means = sapply(list_result, function(x){mean(x$recall)})

sort_result = sort(means, index.return = TRUE)
ix = sort_result$ix

session_frame = data.frame()
for(i in 1:length(ix))
{
  temp = data.frame()
  
  recall_prob = list_result[[ix[i]]]$recall
  temp = data.frame(recall = recall_prob , number = rep(i,length(recall_prob)))
  session_frame = rbind(session_frame, temp)
}

p = ggplot(session_frame, aes(x = number, y = recall))  
p = p + geom_point(size =0.7) + theme_bw() + ylim(c(0,1))
#p = p + geom_hline(yintercept = 0, linetype = 'dashed')

p = p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p = p + theme(text = element_text(size=15),
              axis.text.x = element_text(angle=0, hjust=1))
p = p + xlab('Subject Number') + ylab("Predicted Recall") 

ggsave('resid_variablity_list.pdf', plot = p, device = NULL, path = NULL,
       scale = 1, dpi = 500, width = 10, height = 10, units = "cm")


r_vec_sort = r_vec[ix]
r_frame = data.frame(r = r_vec_sort, number = 1:length(r_vec_sort))
r_frame$r = 100*r_frame$r

p = ggplot(r_frame, aes(x = number, y = r))  
p = p + geom_point(size = 0.7) + theme_bw() + ylim(c(0,30))
p = p + geom_hline(yintercept = mean(r_frame$r), linetype = 'dashed')

p = p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p = p + theme(text = element_text(size=15),
              axis.text.x = element_text(angle=0, hjust=1)) 
p = p + xlab('Subject Number') + ylab("Percent Reduction")

ggsave('percent_reduction_list.pdf', plot = p, device = NULL, path = NULL,
       scale = 1, dpi = 500, width = 10, height = 10, units = "cm")


#


ltpfr2_prob = fread('ltpfr2_prob.csv')
ltpfr2_prob = as.data.frame(ltpfr2_prob)
ltpfr2_prob['Sleep'] = ltpfr2_prob['Sleep']
ltpfr2_prob['Alertness'] = ltpfr2_prob['Alertness'] + rnorm(length(ltpfr2_prob[['Alertness']]),0,0.05)
ltpfr2_prob['Sess'] = ltpfr2_prob['session'] 
ltpfr2_prob['session'] = ltpfr2_prob['session'] + 1

#ltpfr2_prob['list'] = factor(ltpfr2_prob[['list']])
#ltpfr2_prob['recalled'] = logit(ltpfr2_prob[['recalled']])
ltpfr2_prob['Time'] = scale(assign_time(ltpfr2_prob[['Time']]))
ltpfr2_prob['Block'] = scale(assign_block(ltpfr2_prob[['list']]))
ltpfr2_prob['list'] = ltpfr2_prob[['list']]
ltpfr2_prob['list_squared'] = ltpfr2_prob[['list']]^2
ltpfr2_prob['recallability'] = (ltpfr2_prob['recallability'])
subjects = unique(ltpfr2_prob[['subject']])


avg_recall_by_list_frame = data.frame()
for(i in 1:length(subjects))
{
  subject = subjects[i]
  indices = which((ltpfr2_prob[['subject']] == subject))
  subject_data = ltpfr2_prob[indices,]
  
  y = subject_data[['recalled']]

  unique_list = unique(subject_data[['list']])
 
  group_by_list = group_by(subject_data, list)
  avg_recall_by_list = summarize(group_by_list, prob_recall = mean(recalled))
  avg_recall_by_list_frame = rbind(avg_recall_by_list_frame, avg_recall_by_list$prob_recall)
  
  Recallability = subject_data[['recallability']]
  
}


group_by_list = group_by(ltpfr2_prob, list)
avg_recall_by_list = summarize(group_by_list, mean = mean(recalled))
avg_se_by_list =  summarize(group_by_list, se = sd(recalled)/sqrt(length(recalled)))


names(avg_recall_by_list_frame) = c(1:24)
avg_recall_mean = apply(avg_recall_by_list_frame,2,mean)
avg_recall_se = apply(avg_recall_by_list_frame,2,function(x){sd(x)/sqrt(length(x))})



avg_recall_frame = data.frame(recall = avg_recall_by_list$mean, list_num = 1:24, se = avg_se_by_list$se)

avg_recall_frame['block'] = assign_block(avg_recall_frame[['list_num']])



p = ggplot(avg_recall_frame, aes(x = list_num, y = recall, group = block)) + geom_point() + xlim(c(0,25.1))
p = p + geom_line() + geom_errorbar(aes( ymin = recall-se, ymax =recall +se, width = 0.2))

p = p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p = p + xlab('List Number') + ylab('Recall Probability')
p = p + theme(text = element_text(size=20))


ggsave('recall_by_list.pdf', plot = p, device = NULL, path = NULL,
       scale = 1, dpi = 500, width = 10, height = 10, units = "cm")



# recallability
quantiles = seq(0,1,length = 25)
recallability = ltpfr2_prob[['recallability']]
quantile_values = quantile(recallability, quantiles)
assign_quantile = function(x)
{
  print(length(x))
  label = rep(0,length(x))
  for(i in 2:25)
  {
    print(i)
    indices_i = which((quantile_values[i-1] <= x) & (x <= quantile_values[i]))
    label[indices_i] = i
  }
  return(label)
}

ltpfr2_prob['quantile'] =assign_quantile(recallability)




group_by_quantile = group_by(ltpfr2_prob, quantile)
avg_recall_by_quantile = summarize(group_by_quantile, mean = mean(recalled))
avg_se_by_quantile =  summarize(group_by_quantile, se = sd(recalled)/sqrt(length(recalled)))
avg_bin = summarize(group_by_quantile, mean = mean(recallability))

avg_recall_frame = data.frame(recall = sort(avg_recall_by_quantile$mean), recallability = avg_bin$mean, se = avg_se_by_quantile$se)



p = ggplot(avg_recall_frame, aes(x =recallability, y = recall) )+ geom_point() + xlim(c(0.475, 0.535))
p = p + geom_line() + geom_errorbar(aes( ymin = recall-se, ymax =recall +se, width = 0.0005))




p = p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p = p + xlab('Avg. Recallability of List') + ylab('Recall Probability')
p = p + theme(text = element_text(size=20))


ggsave('recall_by_avg_recallability.pdf', plot = p, device = NULL, path = NULL,
       scale = 1, dpi = 500, width = 10, height = 10, units = "cm")



