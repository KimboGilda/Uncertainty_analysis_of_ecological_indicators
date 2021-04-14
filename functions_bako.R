# Function for recreating the exaggeration error per indicator and per river

width_exagg_B = function(whiskers_table,x_var, column){

  
  # shoreline
  shoreline_plot_5 = ggplot() + geom_point(data = whiskers_table,aes(x = x_var,y = shoreline_diff, 
                                                                               shape = river)) +
    guides(color=FALSE)+
    labs(x='minimum width (m)') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "") + ylab("Shoreline") + 
    theme(axis.title.x=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  
  # sinuosity
  sinuosity_plot_5 = ggplot() + geom_point(data = whiskers_table,aes(x = x_var,y = sinuosity_diff, 
                                                                     shape = river)) +
    guides(color=FALSE)+
    labs(x='minimum width (m)') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5)  + 
    theme_classic(base_size = 12, base_family = "")+ ylab("Sinuosity") + 
    theme(axis.title.x=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  
  # total sinuosity
  total_sinuosity_plot_5 = ggplot() + geom_point(data = whiskers_table,aes(x = x_var,y = total_sinuosity_diff, 
                                                                     shape = river)) +
    guides(color=FALSE)+
    labs(x='minimum width (m)') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "")+ ylab("Total Sinuosity") + 
    theme(axis.title.x=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  
  
  # number of nodes
  number_of_nodes_plot_5 = ggplot() + geom_point(data = whiskers_table,aes(x = x_var,y = number_of_nodes_diff, 
                                                                     shape = river)) +
    guides(color=FALSE)+
    labs(x='minimum width (m)') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "")+ ylab("Number of Nodes") +
    theme(axis.title.x=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  
  # width variability
  width_variability_plot_5 = ggplot() + geom_point(data = whiskers_table,aes(x = x_var,y = width_variability_diff, 
                                                                     shape = river)) +
    guides(color=FALSE)+
    labs(x='minimum width (m)') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "")+ ylab("Width Variability") +
    theme(axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  
  
    ggarrange(shoreline_plot_5,sinuosity_plot_5,total_sinuosity_plot_5,
              number_of_nodes_plot_5,width_variability_plot_5,ncol = 1)
  }


################# quantiles_plot ###########3
quantiles_plot_B = function(whiskers_table,x_var,column){
  
  shoreline_plot_5 = ggplot() + geom_errorbar(aes(x = rivers_whiskers$sd, ymax = rivers_whiskers$shoreline_90q_diff, ymin = rivers_whiskers$shoreline_10q_diff
                                                  , width = 0.1, color = rivers_whiskers$river)) +
    geom_point(data = rivers_whiskers,aes(x = rivers_whiskers$sd,y = shoreline_diff, 
                                          shape = river)) +
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
                      width = 0.1, color = rivers_whiskers$river)) + 
    geom_point(data = whiskers_table,aes(x = x_var,y = sinuosity_diff)) +
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
                      width = 0.1, color = rivers_whiskers$river)) + 
    geom_point(data = whiskers_table,aes(x = x_var,y = total_sinuosity_diff,shape = river)) +
    guides(color=FALSE)+
    labs(x='sd') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "")+ ylab("Total Sinuosity") + 
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  
  # number of nodes
  number_of_nodes_plot_5 = ggplot() + 
    geom_errorbar(aes(x = x_var, ymax = whiskers_table$number_of_nodes_90q_diff, ymin = whiskers_table$number_of_nodes_10q_diff,
                      width = 0.1, color = rivers_whiskers$river)) + 
    geom_point(data = whiskers_table,aes(x = x_var,y = number_of_nodes_diff, 
                                                                           shape = river)) +
    guides(color=FALSE)+
    labs(x='sd') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5)  + 
    theme_classic(base_size = 12, base_family = "")+ ylab("Number of nodes") +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  
  # width variability
  width_variability_plot_5 = ggplot() + 
    geom_errorbar(aes(x = x_var, ymax = whiskers_table$width_variability_90q_diff, ymin = whiskers_table$width_variability_10q_diff,
                      width = 0.1, color = rivers_whiskers$river)) +
    geom_point(data = whiskers_table,aes(x = x_var,y = width_variability_diff, 
                                                                             shape = river)) +
    guides(color=FALSE)+
    labs(x='sd') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "")+ ylab("width variability") +
    theme(axis.title.y=element_blank(), axis.text.y = element_text(size = 7, face = "bold"),
          axis.text.x = element_text(size = 7, face = "bold"), legend.position = 'none')
  
  
  # pushViewport(viewport(layout = grid.layout(5, 3)))
  #print(shoreline_plot_5, vp = viewport(layout.pos.row = 1, layout.pos.col = column))
  # print(sinuosity_plot_5, vp = viewport(layout.pos.row = 2, layout.pos.col = column))
  # print(total_sinuosity_plot_5, vp = viewport(layout.pos.row = 3, layout.pos.col = column))
  # print(number_of_nodes_plot_5, vp = viewport(layout.pos.row = 4, layout.pos.col = column))
  # print(width_variability_plot_5, vp = viewport(layout.pos.row = 5, layout.pos.col = column))
  # whisker_plot = recordPlot()
  # dev.off()
  # return(whisker_plot)
  ggarrange(shoreline_plot_5,sinuosity_plot_5,total_sinuosity_plot_5,number_of_nodes_plot_5,width_variability_plot_5,ncol = 1)
  }


quantiles_plot_b = function(whiskers_table,x_var,column){
  
  shoreline_plot_5 = ggplot() + geom_point(data = whiskers_table,aes(x = x_var,y = shoreline_diff, 
                                                                     shape = river)) +
    guides(color=FALSE)+
    labs(x='sd') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "") + ylab("Shoreline") + 
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank(),
          legend.position = 'none')
  
  
  # sinuosity
  sinuosity_plot_5 = ggplot() + geom_point(data = whiskers_table,aes(x = x_var,y = sinuosity_diff, 
                                                                     shape = river)) +
    guides(color=FALSE)+
    labs(x='sd') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "")+ ylab("Sinuosity") + 
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank(),
          legend.position = 'none')
  
  # total sinuosity
  total_sinuosity_plot_5 = ggplot() + geom_point(data = whiskers_table,aes(x = x_var,y = total_sinuosity_diff, 
                                                                           shape = river)) +
    guides(color=FALSE)+
    labs(x='sd') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "")+ ylab("Total Sinuosity") + 
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank(),
          legend.position = 'none')
  
  # number of nodes
  number_of_nodes_plot_5 = ggplot() + geom_point(data = whiskers_table,aes(x = x_var,y = number_of_nodes_diff, 
                                                                           shape = river)) +
    guides(color=FALSE)+
    labs(x='sd') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5)  + 
    theme_classic(base_size = 12, base_family = "")+ ylab("Number of nodes") +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank(),
          legend.position = 'none')
  
  # width variability
  width_variability_plot_5 = ggplot() + geom_point(data = whiskers_table,aes(x = x_var,y = width_variability_diff, 
                                                                             shape = river)) +
    guides(color=FALSE)+
    labs(x='sd') + 
    geom_hline(yintercept = 1.00, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.05, linetype = "dashed", alpha = 0.5) + geom_hline(yintercept = 1.10, linetype = "twodash", alpha = 0.5) +
    geom_hline(yintercept = 0.90, linetype = "twodash", alpha = 0.5) + 
    theme_classic(base_size = 12, base_family = "")+ ylab("width variability") +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y = element_blank(),
          legend.position = 'none')
  
  
  # pushViewport(viewport(layout = grid.layout(5, 3)))
  #print(shoreline_plot_5, vp = viewport(layout.pos.row = 1, layout.pos.col = column))
  # print(sinuosity_plot_5, vp = viewport(layout.pos.row = 2, layout.pos.col = column))
  # print(total_sinuosity_plot_5, vp = viewport(layout.pos.row = 3, layout.pos.col = column))
  # print(number_of_nodes_plot_5, vp = viewport(layout.pos.row = 4, layout.pos.col = column))
  # print(width_variability_plot_5, vp = viewport(layout.pos.row = 5, layout.pos.col = column))
  # whisker_plot = recordPlot()
  # dev.off()
  # return(whisker_plot)
  ggarrange(shoreline_plot_5,sinuosity_plot_5,total_sinuosity_plot_5,number_of_nodes_plot_5,width_variability_plot_5,ncol = 1)
  
}


######### Finding differences

find_diff <- function(input) {
  input <- input %>%
    mutate(
      shoreline_diff = ((shoreline - input[1,2])/input[1,2]) + 1,
      sinuosity_diff = ((sinuosity - input[1,3])/input[1,3]) + 1,
      total_sinuosity_diff = ((total_sinuosity - input[1,4])/input[1,4]) + 1,
      number_of_nodes_diff = ((number_of_nodes - input[1,5])/input[1,5]) + 1,
      width_variability_diff = ((width_variability - input[1,6])/input[1,6]) + 1,
      shoreline_10q_diff = ((shoreline_10q - input[1,7])/input[1,7]) + 1, 
      shoreline_90q_diff = ((shoreline_90q - input[1,8])/input[1,8]) + 1,
      sinuosity_10q_diff = ((sinuosity_10q - input[1,9])/input[1,9]) + 1,
      sinuosity_90q_diff = ((sinuosity_90q - input[1,10])/input[1,10]) + 1,
      total_sinuosity_10q_diff = ((total_sinuosity_10q - input[1,11])/input[1,11]) + 1,
      total_sinuosity_90q_diff = ((total_sinuosity_90q - input[1,12])/input[1,12]) + 1,
      number_of_nodes_10q_diff = ((number_of_nodes_10q - input[1,13])/input[1,13]) + 1,
      number_of_nodes_90q_diff = ((number_of_nodes_90q - input[1,14])/input[1,14]) + 1,
      width_variability_10q_diff = ((width_variability_10q - input[1,15])/input[1,15]) + 1,
      width_variability_90q_diff = ((width_variability_90q - input[1,16])/input[1,16]) + 1
      )
  return(input)
}
    
    
    
    