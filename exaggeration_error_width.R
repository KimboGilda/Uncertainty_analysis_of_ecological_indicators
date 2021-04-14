## ----------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(grid)
library(ggplot2)
source('cyrills_r_functions.R')
source('gatherPlots//functions_bako.R')
library(sf)
library(ggpubr)
library(gridExtra)
library(repr)
library(patchwork)


## ----------------------------------------------------------------------------------------------------------------------------------------------
saane <- read_sf("data\\width_extension\\saane_swissimage\\indicators_table.gdb", layer = "indicators_table")
sense <- read_sf("data\\width_extension\\sense_swissimage\\indicators_table.gdb", layer = "indicators_table")
rhone <- read_sf("data\\width_extension\\rhone_swissimage\\indicators_table.gdb", layer = "indicators_table")




rhone = select(rhone,'shoreline','sinuosity','total_sinuosity','number_of_nodes','width_variability')
saane = select(saane,'shoreline','sinuosity','total_sinuosity','number_of_nodes','width_variability')
sense = select(sense,'shoreline','sinuosity','total_sinuosity','number_of_nodes','width_variability')

buffer = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10)
minimum_width = buffer * 2
rhone = cbind(minimum_width,rhone)
saane = cbind(minimum_width,saane)
sense = cbind(minimum_width,sense)

rhone$river = 'rhone'
saane$river = 'saane'
sense$river = 'sense'

rhone <- rhone[!is.na(rhone$shoreline),]
rhone <- rhone[!is.na(rhone$sinuosity),]
rhone <- rhone[!is.na(rhone$total_sinuosity),]
rhone <- rhone[!is.na(rhone$width_variability),]
rhone[is.na(rhone)] <- 0
rhone <- filter(rhone, sinuosity >= 1)
rhone <- filter(rhone, total_sinuosity >= 1)

saane <- saane[!is.na(saane$shoreline),]
saane <- saane[!is.na(saane$sinuosity),]
saane <- saane[!is.na(saane$total_sinuosity),]
saane <- saane[!is.na(saane$width_variability),]
saane[is.na(saane)] <- 0
saane = filter(saane, sinuosity >= 1)
saane = filter(saane, total_sinuosity >= 1)

sense <- sense[!is.na(sense$shoreline),]
sense <- sense[!is.na(sense$sinuosity),]
sense <- sense[!is.na(sense$total_sinuosity),]
sense <- sense[!is.na(sense$width_variability),]
sense[is.na(sense)] <- 0
sense = filter(sense, sinuosity >= 1)
sense = filter(sense, total_sinuosity >= 1)

rhone_originals=c(rhone$shoreline[1],rhone$sinuosity[1],rhone$total_sinuosity[1],rhone$number_of_nodes[1],rhone$width_variability[1])
saane_originals=c(saane$shoreline[1],saane$sinuosity[1],saane$total_sinuosity[1],saane$number_of_nodes[1],saane$width_variability[1])
sense_originals=c(sense$shoreline[1],sense$sinuosity[1],sense$total_sinuosity[1],sense$number_of_nodes[1],sense$width_variability[1])

rhone_whiskers = add_whiskers(rhone,0.05,rhone_originals)
saane_whiskers = add_whiskers(saane,0.05,saane_originals)
sense_whiskers = add_whiskers(sense,0.05,sense_originals)

width_extension_whiskers = rbind(rhone_whiskers,saane_whiskers,sense_whiskers)


## ---- eval=FALSE-------------------------------------------------------------------------------------------------------------------------------
## basic_violin = ggplot(data=rhone_whiskers, aes(x=minimum_width, y=sinuosity)) +
## geom_violin(aes(fill=rhone_whiskers$river)) +
## theme_minimal()
## myplot(width_extension_whiskers,width_extension_whiskers$minimum_width)


## ----find differences--------------------------------------------------------------------------------------------------------------------------
width_extension_whiskers <- width_extension_whiskers %>%
  mutate(
    shoreline_diff = (((shoreline - shoreline_ref) / (shoreline_ref))) + 1,
    sinuosity_diff = (((sinuosity - sinuosity_ref) / (sinuosity_ref))) + 1,
    total_sinuosity_diff = (((total_sinuosity - total_sinuosity_ref) / (total_sinuosity_ref))) + 1,
    number_of_nodes_diff = (((number_of_nodes - number_of_nodes_ref) / (number_of_nodes_ref))) + 1,
    width_variability_diff = (((width_variability - width_variability_ref) / (width_variability_ref))) + 1
  )

width_extension_whiskers$number_of_nodes_diff[is.na(width_extension_whiskers$number_of_nodes_diff)] = 1


## ----------------------------------------------------------------------------------------------------------------------------------------------
width_extension_whisker_plot <- width_exagg_B(width_extension_whiskers, width_extension_whiskers$minimum_width,1)

## ----------------------------------------------------------------------------------------------------------------------------------------------
#





