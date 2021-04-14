### PLOTS 


plotting_ind= function(whiskers_table,x_var,column){
  if(whiskers_table$river == "rhone") {shape = 19} 
  else if(whiskers_table$river == "saane") {shape = 17} 
  else {shape = 15}
  shoreline_plot_5 = ggplot() + geom_errorbar(aes(x = whiskers_table$sd, ymax = whiskers_table$shoreline_90q_diff, ymin = whiskers_table$shoreline_10q_diff
                                                  , width = 0.3)) +
    geom_point(data = whiskers_table,aes(x = whiskers_table$sd,y = shoreline_diff), shape = shape) +
    guides(color=FALSE)+
    labs(x='sd') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "") + ylab("Shoreline") + 
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  
  
  # sinuosity
  sinuosity_plot_5 = ggplot() + 
    geom_errorbar(aes(x = x_var, ymax = whiskers_table$sinuosity_90q_diff, ymin = whiskers_table$sinuosity_10q_diff,
                      width = 0.3)) + 
    geom_point(data = whiskers_table,aes(x = x_var,y = sinuosity_diff), shape = shape) +
    guides(color=FALSE)+
    labs(x='sd') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "")+ ylab("Sinuosity") + 
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  
  # total sinuosity
  total_sinuosity_plot_5 = ggplot() +
    geom_errorbar(aes(x = x_var, ymax = whiskers_table$total_sinuosity_90q_diff, ymin = whiskers_table$total_sinuosity_10q_diff,
                      width = 0.3)) +
    geom_point(data = whiskers_table,aes(x = x_var,y = total_sinuosity_diff), shape = shape) +
    guides(color=FALSE)+
    labs(x='sd') +
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) +
    theme_classic(base_size = 12, base_family = "")+ ylab("Total Sinuosity") +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  # 
  # # number of nodes
  number_of_nodes_plot_5 = ggplot() +
    geom_errorbar(aes(x = x_var, ymax = whiskers_table$number_of_nodes_90q_diff, ymin = whiskers_table$number_of_nodes_10q_diff,
                      width = 0.3)) +
    geom_point(data = whiskers_table,aes(x = x_var,y = number_of_nodes_diff), shape = shape) +
    guides(color=FALSE)+
    labs(x='sd') +
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5)  +
    theme_classic(base_size = 12, base_family = "")+ ylab("Number of Nodes") +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  # 
  # # width variability
  width_variability_plot_5 = ggplot() +
    geom_errorbar(aes(x = x_var, ymax = whiskers_table$width_variability_90q_diff, ymin = whiskers_table$width_variability_10q_diff,
                      width = 0.3)) +
    geom_point(data = whiskers_table,aes(x = x_var,y = width_variability_diff), shape = shape) +
    guides(color=FALSE)+
    labs(x='sd') +
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) +
    theme_classic(base_size = 12, base_family = "")+ ylab("Width Variability") +
    theme(axis.title.y=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  ggarrange(shoreline_plot_5,sinuosity_plot_5, total_sinuosity_plot_5, number_of_nodes_plot_5, width_variability_plot_5, ncol = 1)
}
