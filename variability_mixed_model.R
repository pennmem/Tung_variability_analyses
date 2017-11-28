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

ltpfr2_prob['session'] = scale(ltpfr2_prob['session'] + 1)
#ltpfr2_prob['list'] = factor(ltpfr2_prob[['list']])
ltpfr2_prob['recalled'] = logit(ltpfr2_prob[['recalled']])
ltpfr2_prob['Time'] = scale(assign_time(ltpfr2_prob[['Time']]))
ltpfr2_prob['Block'] = scale(assign_block(ltpfr2_prob[['list']]))
ltpfr2_prob['list'] = factor(assign_list(ltpfr2_prob[['list']]))


model_session = lmer(recalled ~ (1 + Day + Time + Sleep + Alertness + session | subject) + 1 + Day + Time + Sleep + Alertness + session , ltpfr2_prob)

save(model_session, file = 'model_session.rda')
load('model_session.rda')
# get models 
#library(lmerTest)

summary(model_session)

coefs =coef(model_session)$subject


plot_model(coefs[c('Alertness', 'session', 'Sleep', 'Time')], 10, 10, 'intersession_vars.pdf')
plot_model(coefs[colnames(coefs)[2:5]], 10, 10, 'intersession_var_Day.pdf')



coeffs = as.data.frame(summary(model_session)$coefficients)
coeffs['p'] = 2*(1 - pnorm(abs(coeffs[,3])))

xtable(coeffs[,1:2], digits = 3)


library(optimx)
model_list =  lmer(recalled ~ (-1 + session + Block + list | subject) + -1 + Block + list +  session + recallability, ltpfr2_prob, control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))








plot(fitted(model), residuals(model))
plot(model)


model_alert = lmer(recalled ~ (1 + Alertness + list| subject) + 1 + list +  Alertness , ltpfr2_prob)
plot(fitted(model_alert), residuals(model_alert))


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

