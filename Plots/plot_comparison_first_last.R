# Random Walk.
# Build CV model and plot it.
# Plot data from file.
# Report fit in terms of r-squared.

library(tidyverse)

fil <- "./input/181015_10mg-kgAMPH_50mM Nimo_2_outlier_scrub.csv"
sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)
dat_list <- split_stims(dat)

plot(dat$time_sec, dat$electrode, type = "l")

vmax <- 1.1
km <- 6.5
pulses <- 30
pulse_freq <- 50
release <- 3.2
bin_size <- 2
electrode_distance <- 1000
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

compare_pulse(dat_list[[1]], fil = fil, vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
              release = release, bin_size = bin_size,
              electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient, convert_current = convert_current,
              calibration_current = calibration_current, calibration_concentration = calibration_concentration)

vmax_min <- 1.1
vmax_max <- 1.1
vmax_by <- 0
km_min <- 6.5
km_max <- 6.5
km_by <- 0
release_min <- 0.2
release_max <- 3.2
release_by <- 1.0

pulses <- 30
pulse_freq <- 50
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

arg_df <- create_arg_df(vmax_min, vmax_max, vmax_by, km_min, 
                        km_max, km_by, 
                        pulses, pulse_freq,
                        release_min, release_max, release_by, bin_size, 
                        electrode_distance, dead_space_distance, diffusion_coefficient, 
                        convert_current, calibration_current, calibration_concentration)

arg_df

fit <- calc_fit_multi(dat_list[[15]], arg_df) 

fit

vmax <- 0.4
km <- 4.5
pulses <- 30
pulse_freq <- 50
release <- 1.0
bin_size <- 2
electrode_distance <- 1000
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

compare_pulse(dat_list[[15]], fil = fil, vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
              release = release, bin_size = bin_size,
              electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient, convert_current = convert_current,
              calibration_current = calibration_current, calibration_concentration = calibration_concentration)
