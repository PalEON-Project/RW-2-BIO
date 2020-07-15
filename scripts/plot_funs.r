# STAN output
get_names <- function(x, varname){
  element_names = sapply(strsplit(names(x), '\\['), function(x) x[[1]])
  return(which(element_names == varname))
}

# NIMBLE output
get_colnames <- function(x, varname){
  col_names = sapply(strsplit(colnames(x), '\\['), function(x) x[[1]])
  return(which(col_names == varname))
}


plot_sig <- function(post, site, mvers, sig_d){
  
  # sig_x_obs = sapply(post, function(x) x[,get_cols(x,'sig_x_obs')])
  sig_x_obs = sapply(post, function(x) x[[get_names(x,'sig_x_obs')]])
  colMeans(sig_x_obs)
  sig_x_obs = melt(sig_x_obs)
  colnames(sig_x_obs) =c('iter', 'model', 'value')
  
  # sig_x = sapply(post, function(x) x[,get_cols(x,'sig_x')])
  sig_x = sapply(post, function(x) x[[get_cols(x,'sig_x')]])
  colMeans(sig_x)
  sig_x = melt(sig_x)
  colnames(sig_x) =c('iter', 'model', 'value')
  
  sig_x_sum = sig_x
  sig_x_sum$value = sig_x$value + sig_x_obs$value
  
  sig_rw = rbind(data.frame(sig_x_obs, par=rep('sig_x_obs')),
                 data.frame(sig_x, par=rep('sig_x')),
                 data.frame(sig_x_sum, par=rep('sig_x_sum')))
  
  # sig_rw$model = models[sig_rw$model]
  
  ggplot(data=sig_rw) + 
    geom_line(aes(x=iter, y=value, colour=factor(model))) + 
    facet_grid(par~., scales="free_y") + 
    theme_bw() +
    labs(colour='Data')
  ggsave(file=paste0('sites/', site, '/figures/sig_x_trace_', mvers, '.pdf'))
  
  sig_rw = sig_rw[sig_rw$par != 'sig_x_sum',]
  
  ggplot(data=sig_rw)+#, aes(x=value, y=..scaled..)) + #geom_histogram(aes(value, colour=model, fill=model), binwidth=0.02, stat='bin') + 
    geom_density(aes(x=value, colour=factor(model), fill=factor(model)),alpha=.2)+ 
    facet_grid(par~., scales="free_y") + 
    labs(colour='Method') + 
    labs(fill='Method') + ylab('Density') + xlab('Value') + 
    theme_bw() +
    theme(strip.text = element_text(size = 12), legend.text = element_text(size=12),legend.title = element_text(size=12), axis.title = element_text(size=12), axis.text = element_text(size=12))
  # theme(strip.text = element_text(size = 12))
  ggsave(file=paste0('sites/', site, '/figures/sig_x_density_', mvers, '.pdf'))
  
  if (sig_d){
    sig_d_obs = sapply(post, function(x) x[,get_cols(x,'sig_d_obs')])
    colMeans(sig_d_obs)
    sig_d = melt(sig_d_obs)
    colnames(sig_d) =c('iter', 'model', 'value')
    
    sig_d = data.frame(sig_d, par=rep('sig_d_obs'))
    
    # sig_d$model = models[sig_d$model]
    
    ggplot(data=sig_d) + geom_line(aes(x=iter, y=value, colour=factor(model))) + facet_grid(par~., scales="free_y") + 
      labs(colour='Data')
    ggsave(file=paste0('sites/', site, '/figures/sig_d_trace_', mvers, '.pdf'))
    
    
    # ggplot(data=sig_d) + geom_histogram(aes(value), stat='bin')
    # ggsave(file=paste0('figures/sig_d_hist_', suff ,'.pdf'))
    
    ggplot(data=sig_d)+#, aes(x=value, y=..scaled..)) + #geom_histogram(aes(value, colour=model, fill=model), binwidth=0.02, stat='bin') + 
      geom_density(aes(x=value,  colour=factor(model), fill=factor(model)),alpha=.2)+ 
      facet_grid(par~., scales="free_y") + 
      labs(colour='Method') + 
      labs(fill='Method') + ylab('Density') + xlab('Value') + 
      xlim(c(0,0.02)) + 
      theme(strip.text = element_text(size = 12), legend.text = element_text(size=12),legend.title = element_text(size=12), axis.title = element_text(size=12), axis.text = element_text(size=12))
    ggsave(file=paste0('sites/', site, '/figures/sig_d_density_', mvers, '.pdf'))
  }
  
  # all_sig = rbind(sig_rw, sig_d)
  # 
  # sig_fig <- ggplot(data=all_sig, aes(x=value, y=..scaled..)) + #geom_histogram(aes(value, colour=model, fill=model), binwidth=0.02, stat='bin') + 
  #   geom_density(data=all_sig, aes( colour=factor(model), fill=factor(model)),alpha=.2)+ 
  #   facet_wrap(~par, nrow=3, dir="v", strip.position="right")+#, scales="free_x", ) + 
  #   labs(colour='Method') + 
  #   labs(fill='Method') + ylab('Density') + xlab('Value') + theme_bw() + 
  #   theme(strip.text = element_text(size = 12), legend.text = element_text(size=12),
  #         legend.title = element_text(size=12), axis.title = element_text(size=12), axis.text = element_text(size=12),
  #         strip.background = element_rect(colour = NA)) #+ theme(legend.position="none")
  # print(sig_fig)
  # 
  # ggsave(file=paste0('sites/', site, '/figures/sig_pars_density_', mvers, '.pdf'))
  # ggsave(file=paste0('sites/', site, '/figures/sig_pars_density_', mvers, '.png'))
  # 
  
  
}