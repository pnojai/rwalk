library(ggplot2)
library(dplyr)
library(data.table)

pipeline_dir <- "/media/sf_OneDrive_-_cumc.columbia.edu/rwalk/pipeline"
input_dir <- paste(pipeline_dir, "06_Library", sep = "/")# WT - Pre-AMPH
output_dir <- paste(pipeline_dir, "output", "plots", sep = "/")
dat_fil <- "stim_df.csv"
stim_df <- fread(paste(input_dir, dat_fil, sep = "/"))

stim_df$electrode <- stim_df$electrode_concentration

# WT - Pre-AMPH
dat_merge_wt_pre <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode))
dat_merge_wt_pre <- rename(dat_merge_wt_pre, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
qplot(dat_merge_wt_pre$time_sec, dat_merge_wt_pre$electrode, geom = "line")

# WT - AMPH_06_10
dat_merge_wt_amph_0610 <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 6 & stimulus <= 10 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode))
dat_merge_wt_amph_0610 <- rename(dat_merge_wt_amph_0610, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
qplot(dat_merge_wt_amph_0610$time_sec, dat_merge_wt_amph_0610$electrode, geom = "line")

# WT - AMPH_16_20
dat_merge_wt_amph_1620 <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 16 & stimulus <= 20 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode))
dat_merge_wt_amph_1620 <- rename(dat_merge_wt_amph_1620, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
qplot(dat_merge_wt_amph_1620$time_sec, dat_merge_wt_amph_1620$electrode, geom = "line")

# KO - Pre-AMPH
dat_merge_ko_pre <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "ko" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode))
dat_merge_ko_pre <- rename(dat_merge_ko_pre, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
qplot(dat_merge_ko_pre$time_sec, dat_merge_ko_pre$electrode, geom = "line")

# KO - AMPH_06_10
dat_merge_ko_amph_0610 <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "ko" & stimulus >= 6 & stimulus <= 10 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode))
dat_merge_ko_amph_0610 <- rename(dat_merge_ko_amph_0610, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
qplot(dat_merge_ko_amph_0610$time_sec, dat_merge_ko_amph_0610$electrode, geom = "line")

# KO - AMPH_16_20
dat_merge_ko_amph_1620 <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 16 & stimulus <= 20 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode))
dat_merge_ko_amph_1620 <- rename(dat_merge_ko_amph_1620, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
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

# Variables
genotype <- "WT"
amphetamine <- "PRE"
release <- 1.9
vmax <- 4.8
km <- 5
base_tolerance <- 0.05
plot_duration_sec = 18

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
              plot_duration_sec = plot_duration_sec,
              dead_space = dead_space_distance)

# WT - Post-AMPH 6-10

# Variables
genotype <- "WT"
amphetamine <- "POST_06-10"
release <- 5.9
vmax <- 4.8
km <- 24
base_tolerance <- 0.1
plot_duration_sec = 60

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
              plot_duration_sec = plot_duration_sec,
              dead_space = dead_space_distance)

# WT - Post-AMPH 16-20

# Variables
genotype <- "WT"
amphetamine <- "POST_16-20"
release <- 4.3
vmax <- 4.8
km <- 24
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
              plot_duration_sec = plot_duration_sec,
              dead_space = dead_space_distance)

# ----------------- END OF WILD TYPE -----------------#

# ----------------- BEGIN KNOCKOUT -----------------#

# KO - Pre-AMPH

# Variables
genotype <- "KO"
amphetamine <- "PRE"
release <- 1.83
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
              plot_duration_sec = plot_duration_sec,
              dead_space = dead_space_distance)

# KO - Post-AMPH 6-10

# Variables
genotype <- "KO"
amphetamine <- "POST_06-10"
release <- 4.1
vmax <- 4.8
km <- 24
base_tolerance <- 0.05
plot_duration_sec = 50

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
              plot_duration_sec = plot_duration_sec,
              dead_space = dead_space_distance)

# KO - Post-AMPH 16-20

# Variables
genotype <- "KO"
amphetamine <- "POST_16-20"
release <- 4.3
vmax <- 4.8
km <- 24
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
              plot_duration_sec = plot_duration_sec,
              dead_space = dead_space_distance)

results

