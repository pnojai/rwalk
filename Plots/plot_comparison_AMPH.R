# Random Walk.
# Build CV model and plot it.
# Plot data from file.
# Report fit in terms of r-squared.

library(tidyverse)

fil <- "input/181015_10mg-kgAMPH_50mM Nimo_2_1st_stim.csv"
sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)

vmax <- 1.1
km <- 6.5
pulses <- 30
pulse_freq <- 50
release <- 3.2
bin_size <- 2
electrode_distance <- 1000
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
smoothing_count <- 4
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

compare_pulse(dat, fil = fil, vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
              release = release, bin_size = bin_size,
              electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient, smoothing_count = smoothing_count, convert_current = convert_current,
              calibration_current = calibration_current, calibration_concentration = calibration_concentration)
