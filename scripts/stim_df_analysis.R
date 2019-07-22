library(ggplot2)
library(dplyr)
library(data.table)

# Global variables
pipeline_dir <- "./pipeline"
input_dir <- paste(pipeline_dir, "06_Library", sep = "/")

dat_fil <- "stim_df.csv"
stim_df <- fread(paste(input_dir, dat_fil, sep = "/"))

# Demo copying of raw data.
head(stim_df)
animal_1902051 <- select(stim_df, animal, time_sec, electrode_current) %>%
        filter(animal == 1902051)

# # WT - Pre-AMPH
# dat_merge_wt_pre <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
#         filter(genotype == "wt" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 120) %>%
#         group_by(stim_time_sec) %>%
#         summarize(mean(electrode_concentration))
# dat_merge_wt_pre <- rename(dat_merge_wt_pre, time_sec = stim_time_sec, "electrode_concentration" = "mean(electrode_concentration)")
# qplot(dat_merge_wt_pre$time_sec, dat_merge_wt_pre$electrode_concentration, geom = "line")

# # WT - AMPH_06_10
# dat_merge_wt_amph_0610 <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
#         filter(genotype == "wt" & stimulus >= 6 & stimulus <= 10 & include == TRUE & stim_time_sec <= 120) %>%
#         group_by(stim_time_sec) %>%
#         summarize(mean(electrode))
# dat_merge_wt_amph_0610 <- rename(dat_merge_wt_amph_0610, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
# qplot(dat_merge_wt_amph_0610$time_sec, dat_merge_wt_amph_0610$electrode, geom = "line")
# 
# # WT - AMPH_16_20
# dat_merge_wt_amph_1620 <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
#         filter(genotype == "wt" & stimulus >= 16 & stimulus <= 20 & include == TRUE & stim_time_sec <= 120) %>%
#         group_by(stim_time_sec) %>%
#         summarize(mean(electrode))
# dat_merge_wt_amph_1620 <- rename(dat_merge_wt_amph_1620, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
# qplot(dat_merge_wt_amph_1620$time_sec, dat_merge_wt_amph_1620$electrode, geom = "line")
# 
# # KO - Pre-AMPH
# find the noise
noise <- select(stim_df, animal, genotype, stimulus, include, electrode_concentration) %>%
        filter(genotype == "ko" & stimulus >= 1 & stimulus <= 3 & include == TRUE & electrode_concentration > 3)

animal_1905301 <- select(stim_df, animal, stimulus, time_sec, electrode_concentration, include) %>%
        filter(animal == 1905301 & include == TRUE & stimulus >= 1 & stimulus <= 3)

qplot(animal_1905301$time_sec, animal_1905301$electrode_concentration, geom = "line")

dat_merge_ko_pre <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
        filter(genotype == "ko" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode_concentration))
dat_merge_ko_pre <- rename(dat_merge_ko_pre, time_sec = stim_time_sec, "electrode" = "mean(electrode_concentration)")
qplot(dat_merge_ko_pre$time_sec, dat_merge_ko_pre$electrode, geom = "line")

# dat_merge_ko_pre <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
#         filter(genotype == "ko" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 120) %>%
#         group_by(stim_time_sec) %>%
#         summarize(mean(electrode))
# dat_merge_ko_pre <- rename(dat_merge_ko_pre, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
# qplot(dat_merge_ko_pre$time_sec, dat_merge_ko_pre$electrode, geom = "line")
# 
# # KO - AMPH_06_10
# dat_merge_ko_amph_0610 <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
#         filter(genotype == "ko" & stimulus >= 6 & stimulus <= 10 & include == TRUE & stim_time_sec <= 120) %>%
#         group_by(stim_time_sec) %>%
#         summarize(mean(electrode))
# dat_merge_ko_amph_0610 <- rename(dat_merge_ko_amph_0610, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
# qplot(dat_merge_ko_amph_0610$time_sec, dat_merge_ko_amph_0610$electrode, geom = "line")
# 
# # KO - AMPH_16_20
# dat_merge_ko_amph_1620 <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
#         filter(genotype == "wt" & stimulus >= 16 & stimulus <= 20 & include == TRUE & stim_time_sec <= 120) %>%
#         group_by(stim_time_sec) %>%
#         summarize(mean(electrode))
# dat_merge_ko_amph_1620 <- rename(dat_merge_ko_amph_1620, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
# qplot(dat_merge_ko_amph_1620$time_sec, dat_merge_ko_amph_1620$electrode, geom = "line")
# 
# # Plot and compile results.
# results <- data.frame(genotype = character(),
#                       amphetamine = character(),
#                       release = double(),
#                       vmax = double(),
#                       km = double(),
#                       stringsAsFactors = FALSE)
# 
# Constants
pulses <- 30
pulse_freq <- 50
bin_size <- 2.0
electrode_distance <- 1000
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current <- FALSE
fit_region = "fall"

# # WT - Pre-AMPH
# 
# # Variables
# genotype <- "WT"
# amphetamine <- "PRE"
# release <- 2.25
# vmax <- 4.8
# km <- 5
# base_tolerance <- 0.05
# plot_duration_sec = 18
# 
# if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
#         results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
# } else {
#         results[results$genotype == genotype & 
#                         results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
# }
# 
# compare_pulse(dat = dat_merge_wt_pre, fil = "Wild Type - Pre-AMPH",
#               vmax = vmax, km = km,
#               pulses = pulses,
#               pulse_freq = pulse_freq,
#               release = release,
#               bin_size = bin_size,
#               electrode_distance = electrode_distance,
#               dead_space_distance = dead_space_distance,
#               diffusion_coefficient = diffusion_coefficient,
#               convert_current = convert_current,
#               fit_region = fit_region,
#               base_tolerance = base_tolerance,
#               plot_duration_sec = plot_duration_sec)
# 
# # WT - Post-AMPH 6-10
# 
# # Variables
# genotype <- "WT"
# amphetamine <- "POST_06-10"
# release <- 5.5
# vmax <- 4.8
# km <- 24
# base_tolerance <- 0.05
# plot_duration_sec = 50
# 
# if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
#         results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
# } else {
#         results[results$genotype == genotype & 
#                         results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
# }
# 
# compare_pulse(dat = dat_merge_wt_amph_0610, fil = "Wild Type - Post-AMPH 06-10",
#               vmax = vmax, km = km,
#               pulses = pulses,
#               pulse_freq = pulse_freq,
#               release = release,
#               bin_size = bin_size,
#               electrode_distance = electrode_distance,
#               dead_space_distance = dead_space_distance,
#               diffusion_coefficient = diffusion_coefficient,
#               convert_current = convert_current,
#               fit_region = fit_region,
#               base_tolerance = base_tolerance,
#               plot_duration_sec = plot_duration_sec)
# 
# # WT - Post-AMPH 16-20
# 
# # Variables
# genotype <- "WT"
# amphetamine <- "POST_16-20"
# release <- 4.3
# vmax <- 4.8
# km <- 24
# base_tolerance <- 0.05
# plot_duration_sec = 50
# 
# if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
#         results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
# } else {
#         results[results$genotype == genotype & 
#                         results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
# }
# 
# compare_pulse(dat = dat_merge_wt_amph_1620, fil = "Wild Type - Post-AMPH 16-20",
#               vmax = vmax, km = km,
#               pulses = pulses,
#               pulse_freq = pulse_freq,
#               release = release,
#               bin_size = bin_size,
#               electrode_distance = electrode_distance,
#               dead_space_distance = dead_space_distance,
#               diffusion_coefficient = diffusion_coefficient,
#               convert_current = convert_current,
#               fit_region = fit_region,
#               base_tolerance = base_tolerance,
#               plot_duration_sec = plot_duration_sec)
# 
# # ----------------- END OF WILD TYPE -----------------#
# 
# # ----------------- BEGIN KNOCKOUT -----------------#
# 
# KO - Pre-AMPH

# Variables
genotype <- "KO"
amphetamine <- "PRE"
release <- 1.77
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

# # KO - Post-AMPH 6-10
# 
# # Variables
# genotype <- "KO"
# amphetamine <- "POST_06-10"
# release <- 3.8
# vmax <- 4.8
# km <- 24
# base_tolerance <- 0.05
# plot_duration_sec = 30
# 
# if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
#         results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
# } else {
#         results[results$genotype == genotype & 
#                         results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
# }
# 
# compare_pulse(dat = dat_merge_ko_amph_0610, fil = "Knockout - Post-AMPH 06-10",
#               vmax = vmax, km = km,
#               pulses = pulses,
#               pulse_freq = pulse_freq,
#               release = release,
#               bin_size = bin_size,
#               electrode_distance = electrode_distance,
#               dead_space_distance = dead_space_distance,
#               diffusion_coefficient = diffusion_coefficient,
#               convert_current = convert_current,
#               fit_region = fit_region,
#               base_tolerance = base_tolerance,
#               plot_duration_sec = plot_duration_sec)
# 
# # KO - Post-AMPH 16-20
# 
# # Variables
# genotype <- "KO"
# amphetamine <- "POST_16-20"
# release <- 4.3
# vmax <- 4.8
# km <- 24
# base_tolerance <- 0.05
# plot_duration_sec = 50
# 
# if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
#         results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
# } else {
#         results[results$genotype == genotype & 
#                         results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
# }
# 
# compare_pulse(dat = dat_merge_ko_amph_1620, fil = "Knockout - Post-AMPH 16-20",
#               vmax = vmax, km = km,
#               pulses = pulses,
#               pulse_freq = pulse_freq,
#               release = release,
#               bin_size = bin_size,
#               electrode_distance = electrode_distance,
#               dead_space_distance = dead_space_distance,
#               diffusion_coefficient = diffusion_coefficient,
#               convert_current = convert_current,
#               fit_region = fit_region,
#               base_tolerance = base_tolerance,
#               plot_duration_sec = plot_duration_sec)
# 
# results
# 

dat <- select(stim_df, animal, stim_time_sec, time_sec, electrode_concentration, electrode_current, genotype, stimulus, include) %>%
        filter(animal == 1902051)
# group_by(stim_time_sec) %>%
# summarize(mean(electrode_concentration))
dat_merge_ko_pre <- rename(dat_merge_ko_pre, time_sec = stim_time_sec, "electrode_concentration" = "mean(electrode_concentration)")
qplot(dat_merge_ko_pre$time_sec, dat_merge_ko_pre$electrode_concentration, geom = "line")

# Plot peak values for each stimulus for an animal
unique(stim_df$animal)

dat_one_animal_peaks <- select(stim_df, animal, stimulus, include, time_sec, electrode_concentration) %>%
        filter(animal == 1905302, include == TRUE ) %>%
        group_by(stimulus) %>%
        summarize(max(electrode_concentration))

qplot(dat_one_animal_peaks$stimulus, dat_one_animal_peaks$`max(electrode_concentration)`, geom = "line")
