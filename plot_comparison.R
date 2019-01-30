library(tidyverse)

fil <- "Data/AMPH_test.csv"
sample_rate <- 100
vmax <- 4.57
km <- 0.78
release <- 6000 # 2.75
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
smoothing_count <- 4

compare(fil = fil, sample_rate = sample_rate, vmax = vmax, km = km,
        release = release, bin_size = bin_size,
        electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
        diffusion_coefficient = diffusion_coefficient, smoothing_count = smoothing_count)