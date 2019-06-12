sample_rate <- 100
input_dir <- "./input"
output_dir <- "./output"
lead_time_sec <- 10
win_length_sec <- 120

# Build file list from input directory
fils <- list.files(path = input_dir, pattern = "*.csv", full.names = FALSE)

# Verify segments for all files
for (i in 1:(length(fils) - 0)) {
        dat <- read_experiment_csv(paste(input_dir, fils[i], sep = "/"), sr = sample_rate)
        verify_segments_gg(dat, fils[i], lead_time_sec, win_length_sec)
}

# Split all files
for (i in 1:(length(fils) - 0)) {
        dat <- read_experiment_csv(paste(input_dir, fils[i], sep = "/"), sr = sample_rate)
        dat_list <- split_stims(dat, lead_time_sec = lead_time_sec, win_length_sec = win_length_sec, sr = sample_rate)
        
        for (j in 1:length(dat_list)) {
                fn <- paste0(strsplit(fils[i], "\\.csv"), "_", j, ".csv")
                write.csv(dat_list[[j]], paste0(output_dir, "/", fn), row.names = FALSE)
        }
}

# Manually move them to input directory
# Now plot the fronts
# Plot the front and scrub, trimming the lead time.
library(ggplot2)
fils <- list.files(path = output_dir, pattern = "csv$", full.names = FALSE)
for (i in 1:(length(fils) - 0)) {
        dat <- read.csv(paste(output_dir, fils[i], sep = "/"))
        #start_ix <- 80
        #dat <- dat[start_ix:(start_ix + 90), ]
        p <- qplot(dat$time_sec, dat$electrode, geom = "line", main = fils[i])
        print(p)
}

# Parameters
# Set model parameters
vmax <- 4.8
km <- 2.1
pulses <- 30
pulse_freq <- 50
release <- 2.4
bin_size <- 2
electrode_distance <- 1000
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 10362 # For  files dated 180430
calibration_concentration = 5.0
fit_region = "f"
base_tolerance <- 0.05
plot_duration_sec = 10

fils <- list.files(path = input_dir, pattern = "*.csv", full.names = FALSE)

for (i in 1:(length(fils) - 0)) {
        dat <- read.csv(paste(input_dir, fils[i], sep = "/"))
        
        compare_pulse(dat = dat, fil = fils[i], vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
                      release = release, bin_size = bin_size,
                      electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
                      diffusion_coefficient = diffusion_coefficient, convert_current = convert_current,
                      calibration_current = calibration_current, calibration_concentration = calibration_concentration,
                      fit_region = fit_region, base_tolerance = base_tolerance, plot_duration_sec = plot_duration_sec)
        
}

dat <- read.csv(paste(input_dir, fils[1], sep = "/"))

compare_pulse(dat = dat, fil = fils[1], vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
              release = release, bin_size = bin_size,
              electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient, convert_current = convert_current,
              calibration_current = calibration_current, calibration_concentration = calibration_concentration,
              fit_region = fit_region, base_tolerance = base_tolerance, plot_duration_sec = plot_duration_sec)
