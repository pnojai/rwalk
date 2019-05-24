sample_rate <- 100
input_dir <- "./input"
lead_time_sec <- 9
win_length_sec <- 119.5

# Build file list from input directory
fils <- list.files(path = input_dir, pattern = "*.csv", full.names = TRUE)

# Verify segments for all files
for (i in 1:(length(fils) - 0)) {
        dat <- read_experiment_csv(fils[i], sr = sample_rate)
        verify_segments_gg(dat, fils[i], lead_time_sec, win_length_sec)
}

# Set model parameters
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

# Plot all stimuli from first file
# dat <- read_experiment_csv(fils[1], sr = sample_rate)
# dat_list <- split_stims(dat, lead_time_sec = lead_time_sec, win_length_sec = win_length_sec)
# 
# for (i in 1:length(dat_list)) {
#         compare_pulse(dat = dat_list[[i]], fil = paste0(fils[1],": ", i), vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
#                       release = release, bin_size = bin_size,
#                       electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
#                       diffusion_coefficient = diffusion_coefficient, convert_current = convert_current,
#                       calibration_current = calibration_current, calibration_concentration = calibration_concentration,
#                       fit_region = fit_region, base_tolerance = base_tolerance, plot_duration_sec = plot_duration_sec)
# }

# Plot all files
for (i in 1:(length(fils) - 0)) {
        dat <- read_experiment_csv(fils[i], sr = sample_rate)
        dat_list <- split_stims(dat, lead_time_sec = lead_time_sec, win_length_sec = win_length_sec)
        
        for (j in 1:length(dat_list)) {
                compare_pulse(dat = dat_list[[j]], fil = paste0(fils[i],": ", j), vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
                              release = release, bin_size = bin_size,
                              electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
                              diffusion_coefficient = diffusion_coefficient, convert_current = convert_current,
                              calibration_current = calibration_current, calibration_concentration = calibration_concentration,
                              fit_region = fit_region, base_tolerance = base_tolerance, plot_duration_sec = plot_duration_sec)
        }
        
}