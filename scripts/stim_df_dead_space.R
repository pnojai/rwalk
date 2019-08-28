library(ggplot2)
library(dplyr)
library(data.table)

# Global variables
pipeline_dir <- "/media/sf_OneDrive_-_cumc.columbia.edu/rwalk/pipeline"
input_dir <- paste(pipeline_dir, "06_Library", sep = "/")# WT - Pre-AMPH

# Constants
pulses <- 30
pulse_freq <- 50
bin_size <- 2.0
electrode_distance <- 1000
diffusion_coefficient <- 2.7 * 10^-6
convert_current <- FALSE
fit_region = "fall"
plot_duration_sec = 40

dat_fil <- "stim_df.csv"
stim_df <- fread(paste(input_dir, dat_fil, sep = "/"))

plot_params <- stim_df %>%
                distinct(animal, stimulus, include) %>%
                filter(include == TRUE) %>%
                arrange(animal, stimulus)
plot_params <- plot_params[ , -3]
plot_params <- cbind(plot_params,
                     "release" = as.double(NA),
                     "vmax" = as.double(NA),
                     "km" = as.double(NA),
                     "dead_space_distance" = as.double(NA),
                     "base_tolerance" = as.double(NA)
)

animals <- unique(plot_params$animal)
animals

# Starter with NA for params.
# write.csv(plot_params, paste(input_dir, "dead_space_params.csv", sep = "/"))

# Pick off one animal.
an_animal <- animals[1]
dat <- stim_df %>%
        filter(animal == an_animal & include == TRUE & stim_time_sec <= 120)
qplot(dat$time_sec, dat$electrode_concentration, geom = "line")

# Fit one stim.
a_stim <- 1
dat_fit <- select(dat, stimulus, stim_time_sec, electrode_concentration) %>%
        filter(stimulus == a_stim)
dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
dat_fit <- dat_fit[ , -1]

# Variables
release <- 3.9
vmax <- 4.8
km <- 3
dead_space_distance <- 12
base_tolerance <- 0.05

compare_pulse(dat = dat_fit, fil = paste(an_animal, a_stim),
              vmax = vmax, km = km,
              pulses = pulses,
              pulse_freq = pulse_freq,
              release = release,
              bin_size = bin_size,
              electrode_distance = electrode_distance,
              dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient,
              convert_current = convert_current,
              fit_region = fit_region,
              base_tolerance = base_tolerance,
              plot_duration_sec = plot_duration_sec)

# Fit one stim.
a_stim <- 2 # Bad baseline.
dat_fit <- select(dat, stimulus, stim_time_sec, electrode_concentration) %>%
        filter(stimulus == a_stim)
dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
dat_fit <- dat_fit[ , -1]

# Variables
release <- 3.9
vmax <- 4.8
km <- 3
dead_space_distance <- 12
base_tolerance <- 0.05
plot_duration_sec = 10

compare_pulse(dat = dat_fit, fil = paste(an_animal, a_stim),
              vmax = vmax, km = km,
              pulses = pulses,
              pulse_freq = pulse_freq,
              release = release,
              bin_size = bin_size,
              electrode_distance = electrode_distance,
              dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient,
              convert_current = convert_current,
              fit_region = fit_region,
              base_tolerance = base_tolerance,
              plot_duration_sec = plot_duration_sec)

# Fit one stim.
a_stim <- 3
dat_fit <- select(dat, stimulus, stim_time_sec, electrode_concentration) %>%
        filter(stimulus == a_stim)
dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
dat_fit <- dat_fit[ , -1]

# Variables
release <- 4.8
vmax <- 4.8
km <- 3.5
dead_space_distance <- 14
base_tolerance <- 0.05
plot_duration_sec = 10

compare_pulse(dat = dat_fit, fil = paste(an_animal, a_stim),
              vmax = vmax, km = km,
              pulses = pulses,
              pulse_freq = pulse_freq,
              release = release,
              bin_size = bin_size,
              electrode_distance = electrode_distance,
              dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient,
              convert_current = convert_current,
              fit_region = fit_region,
              base_tolerance = base_tolerance,
              plot_duration_sec = plot_duration_sec)

# Fit one stim.
a_stim <- 4
dat_fit <- select(dat, stimulus, stim_time_sec, electrode_concentration) %>%
        filter(stimulus == a_stim)
dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
dat_fit <- dat_fit[ , -1]

# Variables
release <- 7
vmax <- 4.8
km <- 5
dead_space_distance <- 14
base_tolerance <- 0.17
plot_duration_sec = 20

compare_pulse(dat = dat_fit, fil = paste(an_animal, a_stim),
              vmax = vmax, km = km,
              pulses = pulses,
              pulse_freq = pulse_freq,
              release = release,
              bin_size = bin_size,
              electrode_distance = electrode_distance,
              dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient,
              convert_current = convert_current,
              fit_region = fit_region,
              base_tolerance = base_tolerance,
              plot_duration_sec = plot_duration_sec)

# Fit one stim.
a_stim <- 5
dat_fit <- select(dat, stimulus, stim_time_sec, electrode_concentration) %>%
        filter(stimulus == a_stim)
dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
dat_fit <- dat_fit[ , -1]

# Variables
release <- 11.5
vmax <- 4.8
km <- 12
dead_space_distance <- 14
base_tolerance <- 0.17
plot_duration_sec = 40

compare_pulse(dat = dat_fit, fil = paste(an_animal, a_stim),
              vmax = vmax, km = km,
              pulses = pulses,
              pulse_freq = pulse_freq,
              release = release,
              bin_size = bin_size,
              electrode_distance = electrode_distance,
              dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient,
              convert_current = convert_current,
              fit_region = fit_region,
              base_tolerance = base_tolerance,
              plot_duration_sec = plot_duration_sec)

# Fit one stim.
a_stim <- 6
dat_fit <- select(dat, stimulus, stim_time_sec, electrode_concentration) %>%
        filter(stimulus == a_stim)
dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
dat_fit <- dat_fit[ , -1]

# Variables
release <- 11.5
vmax <- 4.8
km <- 12
dead_space_distance <- 14
base_tolerance <- 0.17
plot_duration_sec = 40

compare_pulse(dat = dat_fit, fil = paste(an_animal, a_stim),
              vmax = vmax, km = km,
              pulses = pulses,
              pulse_freq = pulse_freq,
              release = release,
              bin_size = bin_size,
              electrode_distance = electrode_distance,
              dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient,
              convert_current = convert_current,
              fit_region = fit_region,
              base_tolerance = base_tolerance,
              plot_duration_sec = plot_duration_sec)

# Fit one stim.
a_stim <- 7
dat_fit <- select(dat, stimulus, stim_time_sec, electrode_concentration) %>%
        filter(stimulus == a_stim)
dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
dat_fit <- dat_fit[ , -1]

# Variables
release <- 11.5
vmax <- 4.8
km <- 12
dead_space_distance <- 14
base_tolerance <- 0.17
plot_duration_sec = 40

compare_pulse(dat = dat_fit, fil = paste(an_animal, a_stim),
              vmax = vmax, km = km,
              pulses = pulses,
              pulse_freq = pulse_freq,
              release = release,
              bin_size = bin_size,
              electrode_distance = electrode_distance,
              dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient,
              convert_current = convert_current,
              fit_region = fit_region,
              base_tolerance = base_tolerance,
              plot_duration_sec = plot_duration_sec)

# Fit one stim.
a_stim <- 9
dat_fit <- select(dat, stimulus, stim_time_sec, electrode_concentration) %>%
        filter(stimulus == a_stim)
dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
dat_fit <- dat_fit[ , -1]

# Variables
release <- 9.8
vmax <- 4.8
km <- 31
dead_space_distance <- 14
base_tolerance <- 0.5
plot_duration_sec = 40

compare_pulse(dat = dat_fit, fil = paste(an_animal, a_stim),
              vmax = vmax, km = km,
              pulses = pulses,
              pulse_freq = pulse_freq,
              release = release,
              bin_size = bin_size,
              electrode_distance = electrode_distance,
              dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient,
              convert_current = convert_current,
              fit_region = fit_region,
              base_tolerance = base_tolerance,
              plot_duration_sec = plot_duration_sec)

# Fit one stim.
a_stim <- 11
dat_fit <- select(dat, stimulus, stim_time_sec, electrode_concentration) %>%
        filter(stimulus == a_stim)
dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
dat_fit <- dat_fit[ , -1]

# Variables
release <- 9.0
vmax <- 4.8
km <- 31
dead_space_distance <- 14
base_tolerance <- 0.5
plot_duration_sec = 40

compare_pulse(dat = dat_fit, fil = paste(an_animal, a_stim),
              vmax = vmax, km = km,
              pulses = pulses,
              pulse_freq = pulse_freq,
              release = release,
              bin_size = bin_size,
              electrode_distance = electrode_distance,
              dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient,
              convert_current = convert_current,
              fit_region = fit_region,
              base_tolerance = base_tolerance,
              plot_duration_sec = plot_duration_sec)

# Fit one stim.
a_stim <- 12
dat_fit <- select(dat, stimulus, stim_time_sec, electrode_concentration) %>%
        filter(stimulus == a_stim)
dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
dat_fit <- dat_fit[ , -1]

# Variables
release <- 8.8
vmax <- 4.8
km <- 28
dead_space_distance <- 14
base_tolerance <- 0.1
plot_duration_sec = 40

compare_pulse(dat = dat_fit, fil = paste(an_animal, a_stim),
              vmax = vmax, km = km,
              pulses = pulses,
              pulse_freq = pulse_freq,
              release = release,
              bin_size = bin_size,
              electrode_distance = electrode_distance,
              dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient,
              convert_current = convert_current,
              fit_region = fit_region,
              base_tolerance = base_tolerance,
              plot_duration_sec = plot_duration_sec)

# Fit one stim.
a_stim <- 13
dat_fit <- select(dat, stimulus, stim_time_sec, electrode_concentration) %>%
        filter(stimulus == a_stim)
dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
dat_fit <- dat_fit[ , -1]

# Variables
release <- 9.0
vmax <- 4.8
km <- 29
dead_space_distance <- 16
base_tolerance <- 0.5
plot_duration_sec = 40

compare_pulse(dat = dat_fit, fil = paste(an_animal, a_stim),
              vmax = vmax, km = km,
              pulses = pulses,
              pulse_freq = pulse_freq,
              release = release,
              bin_size = bin_size,
              electrode_distance = electrode_distance,
              dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient,
              convert_current = convert_current,
              fit_region = fit_region,
              base_tolerance = base_tolerance,
              plot_duration_sec = plot_duration_sec)
