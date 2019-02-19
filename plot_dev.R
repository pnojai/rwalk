library(tidyverse)

fil <- "No file"
sample_rate <- 100
vmax <- 4.57
km <- 0.78
release <- 2.75
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
smoothing_count <- 4

rw <- rwalk_cv_pulse(vmax = vmax, km = km,
               release = release, bin_size = bin_size,
               electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
               diffusion_coefficient = diffusion_coefficient)

rw_electrode <- electrode_results(rw, electrode_pos(rw), smoothing_count = 4)

rw_electrode$src <- "simulation"

plot_rwalk(rw_electrode, fil, release, vmax, km)