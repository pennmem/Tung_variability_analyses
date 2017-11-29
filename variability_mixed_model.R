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

ltpfr2_prob = fread('ltpfr2_prob.csv')
ltpfr2_prob = as.data.frame(ltpfr2_prob)

ltpfr2_prob['Sleep'] = scale(ltpfr2_prob['Sleep'])
ltpfr2_prob['Alertness'] = scale(ltpfr2_prob['Alertness'] + rnorm(length(ltpfr2_prob[['Alertness']]),0,0.05))
ltpfr2_prob['Sess'] = ltpfr2_prob['session'] 

ltpfr2_prob['session'] = scale(ltpfr2_prob['session'] + 1)

#ltpfr2_prob['list'] = factor(ltpfr2_prob[['list']])
ltpfr2_prob['recalled'] = logit(ltpfr2_prob[['recalled']])
ltpfr2_prob['Time'] = scale(assign_time(ltpfr2_prob[['Time']]))
ltpfr2_prob['Block'] = scale(assign_block(ltpfr2_prob[['list']]))
ltpfr2_prob['list'] = factor(assign_list(ltpfr2_prob[['list']]))


model_session = lmer(recalled ~ (1 + Day + Time + Sleep + Alertness + session | subject) + 1 + Day + Time + Sleep + Alertness + session , ltpfr2_prob)

#save(model_session, file = 'model_session.rda')
load('model_session.rda')
# get models 
#library(lmerTest)

summary(model_session)

coefs =coef(model_session)$subject
names(coefs)[9] = 'Session'
names(coefs)[2:5] = c("Mon", "Thu", "Tue", "Wed")

plot_model(coefs[c('Alertness', 'Session', 'Sleep', 'Time')], 10, 10, 'intersession_vars.pdf')
plot_model(coefs[c('Mon', 'Thu', 'Tue', 'Wed')], 10, 10, 'intersession_var_Day.pdf')

coeffs = as.data.frame(summary(model_session)$coefficients)
coeffs['p'] = 2*(1 - pnorm(abs(coeffs[,3])))

xtable(coeffs[,1:2], digits = 3)



residuals = resid(model_list)  # get residuals from each model 
subjects = unique(ltpfr2_prob[['subject']])
r_vec = length(subjects)
list_result = list()
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
  subject_data[['predicted']] = predicted
  
  subject_data_group_by_Sess = group_by(subject_data, Sess)
  recall_prob = summarise( subject_data_group_by_Sess, recall_prob = mean(predicted))
  recall_prob = as.data.frame(recall_prob)
  list_result[i] = list(recall = recall_prob, r = r)
}
means = sapply(list_result, function(x){mean(x$recall)})

sort_result = sort(means, index.return = TRUE)
ix = sort_result$ix

session_frame = data.frame()
for(i in 1:length(ix))
{
  temp = data.frame()
  
  recall_prob = list_result[[ix[i]]]$recall_prob
  temp = data.frame(recall = recall_prob , number = rep(i,length(recall_prob)))
  session_frame = rbind(session_frame, temp)
}

p = ggplot(session_frame, aes(x = number, y = recall))  
p = p + geom_point() + theme_bw()
#p = p + geom_hline(yintercept = 0, linetype = 'dashed')

p = p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p = p + theme(text = element_text(size=15),
              axis.text.x = element_text(angle=0, hjust=1))
p = p + xlab('Subject Number') + ylab("Predicted Recall")

ggsave('residual_variablity_list.pdf', plot = p, device = NULL, path = NULL,
       scale = 1, dpi = 500, width = 10, height = 10, units = "cm")


r_vec_sort = r_vec[ix]
r_frame = data.frame(r = r_vec_sort, number = 1:length(r_vec_sort))

p = ggplot(r_frame, aes(x = number, y = r))  
p = p + geom_point() + theme_bw()
#p = p + geom_hline(yintercept = 0, linetype = 'dashed')

p = p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p = p + theme(text = element_text(size=15),
              axis.text.x = element_text(angle=0, hjust=1))
p = p + xlab('Subject Number') + ylab("Percent Reduction")

ggsave('percent_reduction_list.pdf', plot = p, device = NULL, path = NULL,
       scale = 1, dpi = 500, width = 10, height = 10, units = "cm")


load("model_list.rda")
plot(fitted(model_list), residuals(model_list))
coefs =coef(model_list)$subject
names(coefs)[10] = 'Session'
names(coefs)[11] = 'Recallability'

plot_model(coefs[c('Block', 'Recallability', 'Session')], 10, 10, 'interlists_vars.pdf')


residuals = resid(model_list)  # get residuals from each model 
subjects = unique(ltpfr2_prob[['subject']])
r_vec = length(subjects)
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
  predicted =inv.logit(residuals + mean(y))
}




coeffs = as.data.frame(summary(model_list)$coefficients)
coeffs['p'] = 2*(1 - pnorm(abs(coeffs[,3])))

xtable(coeffs[,1:2], digits = 3)



# model_alert = lmer(recalled ~ (1 + Alertness + list| subject) + 1 + list +  Alertness , ltpfr2_prob)
# plot(fitted(model_alert), residuals(model_alert))

load("model_list.rda")


# analyze ltpfr2 
ltpfr2 = fread('ltpfr2_data_processed.csv')
ltpfr2 = as.data.frame(ltpfr2)

ltpfr2['Sleep'] = scale(ltpfr2['Sleep'])
ltpfr2['Time'] = scale(ltpfr2['Time'])
ltpfr2['Alertness'] = scale(ltpfr2['Alertness'])
ltpfr2['session'] = scale(ltpfr2['session'] + 1)
ltpfr2['list'] = factor(ltpfr2[['list']])
ltpfr2['recalled'] = factor(ltpfr2[['recalled']])
ltpfr2['serial_pos'] = scale(ltpfr2[['serial_pos']])


model = glmer(recalled ~ (1 + Day + Time + Sleep + session |subject) + 1 + Alertness + session + recallability, family = 'binomial', data = ltpfr2 )


model = glmer(recalled ~ (1 + list +  session |subject) + 1 + session , family = 'binomial', data = ltpfr2 )

