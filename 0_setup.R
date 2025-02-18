# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Set up ###########################################################################################################################################
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Libraries
library(tidyverse)
library(ggpubr)
library(RSQLite)
library(dbplyr)
library(terra)
library(plotly)
library(stringr)
library(sf)
library(reshape2)
library(concaveman)
# install.packages('reticulate')                                            # only needs to be run the first time
# reticulate::install_miniconda()                                           # -"-
# reticulate::conda_install('r-reticulate', 'python-kaleido==0.1.*')        # -"-
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')   # -"-
library(reticulate)
reticulate::use_miniconda("r-reticulate") # to save plotly graphs as png
reticulate::py_run_string("import sys")
library(scales)

# Setting helpful variables
"%ni%" <- Negate("%in%")
landscapes <- c("bgd", "grte", "stoko")
vars <- c("size", "freq", "fecundity", "browsing")
areas <- data.frame(area = c(8645, 42586, 35676), landscape = landscapes) # forested landscape area in ha; "bgd", "grte", "stoko"
response.colors <- c("#1b9e77", "#7570b3", "#d95f02")
names(response.colors) <- c("1. Structure\nBasal area decreased by >50 % from reference", "2. Composition\nDominant species changed from reference", "3. Remaining forest\nStem density dropping below 50 trees/ha")
colors.landscape <- c("#009988", "#999933", "#882255")
names(colors.landscape) <- c("Berchtesgaden", "Grand Teton", "Shiretoko")

# data frame with all Resource Units (100x100 m cells) and their elevation, as well as xy-coordinates
rid.df <- read_csv("processed_data/helper_files/rid.df.csv")
