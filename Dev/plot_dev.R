# Random Walk.
# Build CV model and plot it.

library(tidyverse)

vmax <- 4.57
km <- 0.78
release <- 2.75

pulses <- 1
pulse_freq <- 1
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
duration = 2
smoothing_count <- 4

rw <- rwalk_cv_pulse_run(vmax, km, release, pulses, pulse_freq, bin_size, electrode_distance,
                     dead_space_distance, diffusion_coefficient, duration)

# rw_electrode <- electrode_results(rw, electrode_pos(rw), smoothing_count = 4)

# rw_electrode$src <- "simulation"

plot_rwalk_sim(rw_electrode, release, vmax, km)
