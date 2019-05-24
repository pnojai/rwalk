# Random Walk.
# Build CV model and plot it.
# Plot data from file.
# Report fit in terms of r-squared.

library(tidyverse)

fil <- "./input/180430_DA_saline_1.csv"
sample_rate <- 100
dat <- read_experiment_csv(fil, sr = sample_rate)

lead_time_sec <- 9
win_length_sec <- 119.5

verify_segments(dat, lead_time_sec, win_length_sec)

# Perform verified segmentation.
dat_list <- split_stims(dat, lead_time_sec = lead_time_sec, win_length_sec = win_length_sec)

# Model parameters
vmax <- 4.8
km <- 1.7
pulses <- 30
pulse_freq <- 50
release <- 3.35
bin_size <- 2
electrode_distance <- 1000
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0
fit_region = "f"
base_tolerance <- 0.05
plot_duration_sec = 10

# Plot
compare_pulse(dat_list[[5]], fil = fil, vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
              release = release, bin_size = bin_size,
              electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient, convert_current = convert_current,
              calibration_current = calibration_current, calibration_concentration = calibration_concentration,
              fit_region = fit_region, base_tolerance = base_tolerance, plot_duration_sec = plot_duration_sec)

