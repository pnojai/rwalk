library(tidyverse)

fil <- "./Data/181015_10mg-kgAMPH_50mM Nimo_2_outlier_scrub.csv"

sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)
