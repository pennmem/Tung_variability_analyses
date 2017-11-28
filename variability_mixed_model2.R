library(data.table)
library(dplyr)
library(lme4)
library(ggplot2)
source('helper_funcs.R')
library(xtable)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

logit = function(x)
{
  x[which(x==0)] = 0.001
  x[which(x ==1)] = 0.999 
  
  return(log(x/(1-x)))
}

assign_block = function(x)
{
  x[which(x<=8)] = 1
  x[which((8< x) & (x <=16))] =2
  x[which((16<x) & (x<=24))] = 3
  
  return(x)
}

assign_list = function(x)
{
  x[which((8< x) & (x <=16))] = x[which((8< x) & (x <=16))]-8
  x[which((16<x) & (x<=24))] = x[which((16<x) & (x<=24))]-16
  return(x)
}

scale_feature = function(x)
{
  x= (x-min(x))/(max(x)-min(x))
}


assign_time = function(x)
{
  h = 12-(x%%100)/60- floor(x/100)
  return(h)
}


data_path = 'ltpfr2_data_processed.csv'
ltpfr2 = fread(data_path)

ltpfr2 = as.data.frame(ltpfr2)
indices = which((ltpfr2['Day'] != 'Sat') & (ltpfr2['Day'] != 'Sun') )
ltpfr2 = ltpfr2[indices,]
ltpfr2['recalled'] = factor(ltpfr2[['recalled']])
ltpfr2['session'] = scale(ltpfr2[['session']]+1)
ltpfr2['Day'] = factor(ltpfr2[['Day']])
#ltpfr2['Block'] = assign_block(ltpfr2[['list']])
#ltpfr2['list'] = assign_list(ltpfr2[['list']])
ltpfr2['list'] = factor(ltpfr2[['list']])

ltpfr2['Alertness'] = scale(ltpfr2[['Alertness']]+ rnorm(length(ltpfr2[['Alertness']]),0,0.05))


ltpfr2['Time'] = scale(assign_time(ltpfr2[['Time']]))
ltpfr2['Sleep'] = scale(ltpfr2[['Sleep']])
ltpfr2['recallability'] = scale(ltpfr2['recallability'])
#ltpfr2['Block'] = factor(ltpfr2[['Block']])

# correlation table 
xtable(cor(ltpfr2[c('Alertness','recallability', 'Sleep', 'Time')]))





n_total_lists = dim(ltpfr2)[1]/24
subjects = unique(ltpfr2[['subject']])




model_lists = list()
r_squared_vec = c()

for (subject in subjects)
{
  indices_subject = which(ltpfr2[['subject']] == subject)
  subject_data = ltpfr2[indices_subject,]
  # model_subject = glm(recalled ~ serial_pos + list + session +  Alertness + Sleep + Time + Day + list +  recallability -1, data = subject_data, family = 'binomial')
  #model_subject = glm(recalled ~   session +  list + Sleep + Alertness + Time + Day + recallability + serial_pos, data = subject_data, family = 'binomial')
  model_subject = glm(recalled ~ list +  recallability + serial_pos, data = subject_data, family = 'binomial')
  
  model_subject_session = glm(recalled ~   list + recallability + serial_pos, data = subject_data, family = 'binomial')
  null_deviance = summary(model_subject)$null.deviance
  residual_deviance = summary(model_subject)$deviance
  r_squared = 1 - residual_deviance/null_deviance
  
  r_squared_vec = c(r_squared_vec, r_squared)
  model_lists[[subject]] = model_subject
}


# plot coefficients
coeffs_list = lapply(model_lists, function(x){summary(x)$coefficients})

vars_list = c('session', 'Sleep', 'Alertness', 'Time')
result = plot_vars(coeffs_list, vars_list, title = 'intersession.pdf', height = 10, width = 15)


# interlist variability 
coeffs_list = lapply(model_lists, function(x){summary(x)$coefficients})
rownames(coeffs_list[[1]])
vars_list = rownames(coeffs_list[[1]])[3:25]

result= plot_vars(coeffs_list, vars_list, title = 'interlist.pdf', height = 10, width = 15)
result$result

# within list variability 
vars_list = c('serial_pos', 'recallability', '')
result = plot_vars(coeffs_list, vars_list, title = 'intralist.pdf', height = 10, width = 15)
