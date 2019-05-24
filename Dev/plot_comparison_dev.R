library(tidyverse)

fil <- "input/181015_10mg-kgAMPH_50mM Nimo_2_1st_stim.csv"
sample_rate <- 100
vmax <- 0
km <- 0.78
release <- 3.05
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

compare(fil = fil, sample_rate = sample_rate, vmax = vmax, km = km,
        release = release, bin_size = bin_size,
        electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
        diffusion_coefficient = diffusion_coefficient,
        calibration_current = calibration_current, calibration_concentration = calibration_concentration)

compare_pulse(fil = fil, sample_rate = sample_rate, vmax = vmax, km = km,
        release = release, bin_size = bin_size,
        electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
        diffusion_coefficient = diffusion_coefficient,
        calibration_current = calibration_current, calibration_concentration = calibration_concentration)
