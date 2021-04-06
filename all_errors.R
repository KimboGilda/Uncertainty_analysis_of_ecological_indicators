#all errors
library(tidyverse)
library(sf)
library(grid)
library(ggplot2)

# #dev.off()
# pushViewport(viewport(layout = grid.layout(5,3)))
# #print(width_extension_whisker_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
# print(georeferencing_whisker_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
# 
# all_whiskers_plot = recordPlot()
# dev.off()
# all_whiskers_plot


plot1 <- annotate_figure(width_extension_whisker_plot,
                top = text_grob("Exaggeration error", face = "bold", size = 13))

plot2 <- annotate_figure(georeferencing_whisker_plot,
                         top = text_grob("Georeferencing error", face = "bold", size = 13))

plot3 <- annotate_figure(man_dig_whisker_plot,
                         top = text_grob("Manual digitizing error", face = "bold", size = 13))
  
  
allPlots <- ggarrange(plot1,plot2, plot3, ncol = 3, common.legend = TRUE)


