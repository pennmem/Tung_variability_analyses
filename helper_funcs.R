plot_vars = function(coeffs_list, vars_list, title = '', height = 10, width = 10)
{

  vars_list = rownames(coeffs_list[[1]])[3:25]

  inter_session_vars = data.frame()
  inter_session_p_vals = data.frame()
  for(j in 1:length(coeffs_list)){
    coeffs = coeffs_list[[j]]
    inter_session_vars = rbind(inter_session_vars, coeffs[vars_list,][,'Estimate'])
    inter_session_p_vals = rbind(inter_session_p_vals, coeffs[vars_list,][,4])
  }
  
  names(inter_session_vars ) = vars_list
  
  inter_session_vars = apply(inter_session_vars,2, function(x){x-mean(x)})
  
  names(inter_session_vars) = 2:24
  
  names(inter_session_p_vals ) = vars_list
  
  p_adjust = function(x)
  {
    return(p.adjust(x, "fdr"))
  }
  
  inter_session_p_vals = apply(inter_session_p_vals,2, p_adjust)
  
  inter_session_vars_melt = melt(inter_session_vars, ids = vars_list[2:4])
  inter_session_p_vals_melt = melt(inter_session_p_vals)
  inter_session_p_vals_melt['signif'] = (inter_session_p_vals_melt['value'] > 0.05)
  
  inter_session_vars_melt['signif'] = factor(as.numeric(inter_session_p_vals_melt[['signif']]))
  #inter_session_vars = apply(inter_session_vars,2,scale)
  
  
  means = apply(inter_session_vars, 2,mean)
  ses = apply(inter_session_vars,2,sd)
  
  p = ggplot(inter_session_vars_melt, aes(x = variable, y = value))  
  p = p + geom_point(aes( alpha = inter_session_vars_melt[['signif']]), show.legend = F) + theme_bw()

  
  p = p + theme(text = element_text(size=15),
                axis.text.x = element_text(angle=60, hjust=1), axis.title.x = element_blank(), 
                axis.title.y = element_blank()) 

  ggsave(title, plot = p, device = NULL, path = NULL,
         scale = 1, dpi = 500, width = width, height = height, units = "cm")
  
  test_result = apply(inter_session_vars,2,t.test)
  return(list(p =p, result = test_result))
}


plot_model = function(d, height = 10, width = 10, title = '')
{
  names = colnames(d)
  d = apply(d,2,function(x){x -mean(x)})
  colnames(d) = names
  
  d  = melt(d)
  print(names(d))
  
  p = ggplot(d, aes(x = Var2, y = value))  
  p = p + geom_point() + theme_bw()
  #p = p + geom_hline(yintercept = 0, linetype = 'dashed')
  
  p = p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  p = p + theme(text = element_text(size=20),
                axis.text.x = element_text(angle=15, hjust=1), axis.title.x = element_blank(), 
                axis.title.y = element_blank())
  
  ggsave(title, plot = p, device = NULL, path = NULL,
         scale = 1, dpi = 500, width = width, height = height, units = "cm")
  return(p)
}