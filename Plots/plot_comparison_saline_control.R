# Random Walk.
# Build CV model and plot it.
# Plot data from file.
# Report fit in terms of r-squared.

library(tidyverse)

fil <- "./input/180430_DA_saline_1.csv"
sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)
dat_list <- split_stims(dat)

plot(dat$time_sec, dat$electrode, type = "l")

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

compare_pulse(dat_list[[1]], fil = fil, vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
              release = release, bin_size = bin_size,
              electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient, convert_current = convert_current,
              calibration_current = calibration_current, calibration_concentration = calibration_concentration,
              fit_region = fit_region, base_tolerance = base_tolerance)

