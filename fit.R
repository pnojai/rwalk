# Random Walk.
# Build CV model.
# Report fit in terms of r-squared.

library(tidyverse)

fil <- "Data/181015_10mg-kgAMPH_50mM Nimo_2_1_stim.csv"
sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)

vmax_min <- 0.1
vmax_max <- 4.1
vmax_by <- 1
km_min <- 0.5
km_max <- 1.0
km_by <- 0.2
release_min <- 2.5
release_max <- 3.5
release_by <- .5

pulses <- 30
pulse_freq <- 50
bin_size <- 2.0
electrode_distance <- 1000
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
smoothing_count <- 4
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

arg_df <- create_arg_df(vmax_min, vmax_max, vmax_by, km_min, 
                        km_max, km_by, 
                        pulses, pulse_freq,
                        release_min, release_max, release_by, bin_size, 
                        electrode_distance, dead_space_distance, diffusion_coefficient, 
                        smoothing_count, convert_current, calibration_current, calibration_concentration)

arg_df

fit <- calc_fit_multi(dat, arg_df) 

fit
