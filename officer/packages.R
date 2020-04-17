if (!require("officer")) {
  install.packages('officer')
}
##### For Mac users #####
if (!require("Cairo")) {
  install.packages('Cairo')
}

library('officer') # Load
library('magrittr') 
library('dplyr')
library('stringr')
library('ggplot2')