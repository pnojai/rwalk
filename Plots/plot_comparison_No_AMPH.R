# Random Walk.
# Build CV model and plot it.
# Plot data from file.
# Report fit in terms of r-squared.

library(tidyverse)

# fil <- "Data/181015_10mg-kgAMPH_50mM Nimo_2_1_stim.csv"
fil <- "Data/181002_DAinuse.csv"
sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)

# 0.1 0.9    3.05  

vmax <- 1.0
km <- 0.2
pulses <- 30
pulse_freq <- 50
release <- 1.6
bin_size <- 2.0
electrode_distance <- 1000
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
smoothing_count <- 4
convert_current = FALSE
calibration_current = 7500.0
calibration_concentration = 5.0

compare_pulse(dat, vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
        release = release, bin_size = bin_size,
        electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
        diffusion_coefficient = diffusion_coefficient, smoothing_count = smoothing_count, convert_current = convert_current,
        calibration_current = calibration_current, calibration_concentration = calibration_concentration)
