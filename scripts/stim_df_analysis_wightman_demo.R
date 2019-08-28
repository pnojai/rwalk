library(ggplot2)
library(dplyr)
library(data.table)

# Maha's inputs as of 7/30/2019

# Global variables
pipeline_dir <- "./pipeline"
input_dir <- paste(pipeline_dir, "06_Library", sep = "/")# WT - Pre-AMPH

dat_fil <- "stim_df.csv"
stim_df <- fread(paste(input_dir, dat_fil, sep = "/"))

# WT - Pre-AMPH
dat_merge_wt_pre <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode_concentration))
dat_merge_wt_pre <- rename(dat_merge_wt_pre, time_sec = stim_time_sec, "electrode" = "mean(electrode_concentration)")
qplot(dat_merge_wt_pre$time_sec, dat_merge_wt_pre$electrode, geom = "line")

# WT - AMPH_06_10
dat_merge_wt_amph_0610 <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 6 & stimulus <= 10 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode_concentration))
dat_merge_wt_amph_0610 <- rename(dat_merge_wt_amph_0610, time_sec = stim_time_sec, "electrode" = "mean(electrode_concentration)")
qplot(dat_merge_wt_amph_0610$time_sec, dat_merge_wt_amph_0610$electrode, geom = "line")

# WT - AMPH_16_20
dat_merge_wt_amph_1620 <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 16 & stimulus <= 20 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode_concentration))
dat_merge_wt_amph_1620 <- rename(dat_merge_wt_amph_1620, time_sec = stim_time_sec, "electrode" = "mean(electrode_concentration)")
qplot(dat_merge_wt_amph_1620$time_sec, dat_merge_wt_amph_1620$electrode, geom = "line")

# KO - Pre-AMPH
dat_merge_ko_pre <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
        filter(genotype == "ko" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode_concentration))
dat_merge_ko_pre <- rename(dat_merge_ko_pre, time_sec = stim_time_sec, "electrode" = "mean(electrode_concentration)")
qplot(dat_merge_ko_pre$time_sec, dat_merge_ko_pre$electrode, geom = "line")

dat_merge_ko_pre <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
        filter(genotype == "ko" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode_concentration))
dat_merge_ko_pre <- rename(dat_merge_ko_pre, time_sec = stim_time_sec, "electrode" = "mean(electrode_concentration)")
qplot(dat_merge_ko_pre$time_sec, dat_merge_ko_pre$electrode, geom = "line")

# KO - AMPH_06_10
dat_merge_ko_amph_0610 <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
        filter(genotype == "ko" & stimulus >= 6 & stimulus <= 10 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode_concentration))
dat_merge_ko_amph_0610 <- rename(dat_merge_ko_amph_0610, time_sec = stim_time_sec, "electrode" = "mean(electrode_concentration)")
qplot(dat_merge_ko_amph_0610$time_sec, dat_merge_ko_amph_0610$electrode, geom = "line")

# unique(select(stim_df, animal, genotype, include) %>%
#         filter(genotype == "ko" & include == TRUE))
# 
# dat_merge_ko_amph_0610 <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
#         filter(animal == 1905302 & genotype == "ko" & stimulus >= 8 & stimulus <= 8 & include == TRUE & stim_time_sec <= 120) %>%
#         group_by(stim_time_sec) %>%
#         summarize(mean(electrode_concentration))
# dat_merge_ko_amph_0610 <- rename(dat_merge_ko_amph_0610, time_sec = stim_time_sec, "electrode" = "mean(electrode_concentration)")
# qplot(dat_merge_ko_amph_0610$time_sec, dat_merge_ko_amph_0610$electrode, geom = "line")

# Find the extra peak
# KO - AMPH_16_20
dat_merge_ko_amph_1620 <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 16 & stimulus <= 20 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode_concentration))
dat_merge_ko_amph_1620 <- rename(dat_merge_ko_amph_1620, time_sec = stim_time_sec, "electrode" = "mean(electrode_concentration)")
qplot(dat_merge_ko_amph_1620$time_sec, dat_merge_ko_amph_1620$electrode, geom = "line")

# Plot and compile results.
results <- data.frame(genotype = character(),
                      amphetamine = character(),
                      release = double(),
                      vmax = double(),
                      km = double(),
                      stringsAsFactors = FALSE)

# Constants
pulses <- 30
pulse_freq <- 50
bin_size <- 2.0
electrode_distance <- 1000
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current <- FALSE
fit_region = "fall"

# WT - Pre-AMPH

# Maha: 7/30
# Variables
genotype <- "WT"
amphetamine <- "PRE"
release <- 1.8
vmax <- 4.8
km <- 5
base_tolerance <- 0.125
plot_duration_sec = 5

if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
        results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
} else {
        results[results$genotype == genotype &
                        results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
}

compare_pulse(dat = dat_merge_wt_pre, fil = "Wild Type - Pre-AMPH",
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

# WT - Post-AMPH 6-10

# Variables
genotype <- "WT"
amphetamine <- "POST_06-10"
release <- 5.75
vmax <- 4.8
km <- 26
base_tolerance <- 0.5
plot_duration_sec = 25

if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
        results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
} else {
        results[results$genotype == genotype &
                        results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
}

compare_pulse(dat = dat_merge_wt_amph_0610, fil = "Wild Type - Post-AMPH 06-10",
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

# WT - Post-AMPH 16-20

# Variables
genotype <- "WT"
amphetamine <- "POST_16-20"
release <- 4.2
vmax <- 4.8
km <- 32
base_tolerance <- 0.05
plot_duration_sec = 50

if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
        results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
} else {
        results[results$genotype == genotype &
                        results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
}

compare_pulse(dat = dat_merge_wt_amph_1620, fil = "Wild Type - Post-AMPH 16-20",
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

# ----------------- END OF WILD TYPE -----------------#

# ----------------- BEGIN KNOCKOUT -----------------#

# KO - Pre-AMPH

# Variables
genotype <- "KO"
amphetamine <- "PRE"
release <- 1.8
vmax <- 4.8
km <- 5
base_tolerance <- 0.05
plot_duration_sec = 10

if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
        results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
} else {
        results[results$genotype == genotype &
                        results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
}

compare_pulse(dat = dat_merge_ko_pre, fil = "Knockout - Pre-AMPH",
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

# KO - Post-AMPH 6-10

# Variables
genotype <- "KO"
amphetamine <- "POST_06-10"
release <- 4.1
vmax <- 4.8
km <- 26
base_tolerance <- 0.12
plot_duration_sec = 30

if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
        results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
} else {
        results[results$genotype == genotype &
                        results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
}

compare_pulse(dat = dat_merge_ko_amph_0610, fil = "Knockout - Post-AMPH 06-10",
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

# KO - Post-AMPH 16-20

# Variables
genotype <- "KO"
amphetamine <- "POST_16-20"
release <- 4.15
vmax <- 4.8
km <- 32
base_tolerance <- 0.05
plot_duration_sec = 50

if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
        results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
} else {
        results[results$genotype == genotype &
                        results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
}

compare_pulse(dat = dat_merge_ko_amph_1620, fil = "Knockout - Post-AMPH 16-20",
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

results

# Ad hoc
unique(stim_df$animal)
animal_id <- 1902052
# animal_id <- 1809201

dat_adhoc <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
        filter(animal == animal_id & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode_concentration))
dat_adhoc <- rename(dat_adhoc, time_sec = stim_time_sec, "electrode" = "mean(electrode_concentration)")
qplot(dat_adhoc$time_sec, dat_adhoc$electrode, geom = "line")

# Constants
pulses <- 30
pulse_freq <- 50
bin_size <- 2.0
electrode_distance <- 1000
dead_space_distance <- 7
diffusion_coefficient <- 2.7 * 10^-6
convert_current <- FALSE
fit_region = "fall"

# Variables
release <- 1.8
vmax <- 4.8
km <- 3
base_tolerance <- 0.05
plot_duration_sec = 10

compare_pulse(dat = dat_adhoc, fil = "Ad Hoc",
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

# Ad hoc
unique(stim_df$animal)
animal_id <- 1902052
# animal_id <- 1809201

dat_adhoc <- select(stim_df, animal, time_sec, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
        filter(animal == animal_id)

qplot(dat_adhoc$time_sec, dat_adhoc$electrode, geom = "line")
