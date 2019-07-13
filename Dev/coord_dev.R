library(ggplot2)
library(openxlsx)
library(dplyr)
library(data.table)

input_dir <- "./input"           # Input directory, on GitHub

# peakdetection converted to CSV.
# include column added with default to TRUE.

# List of coordinate files.
# I would like a validation routine for presence of data files matching the coordinate files.
# Other way around, too. Each data file has a coordinate file. 
coord_fils <- dir(input_dir, pattern = "*_peakdetection.csv")
dat_fils <- sapply(strsplit(coord_fils, "_peakdetection"), function(x) {
        paste(x, collapse = "")})

#dat <- read_experiment_csv(paste(input_dir, file_name, sep = "/"))
#coord <- fread(paste(input_dir, coord_name, sep = "/"))

# fils <- unique(fil_params_all[ , c("filename", "sample_rate", "animal", "genotype",
#                                    "pulses", "pulse_freq", "bin_size", "electrode_distance",
#                                    "dead_space_distance", "diffusion_coefficient", "convert_current",
#                                    "calibration_current", "calibration_concentration")])
# 
fil_not_exists <- sum(!file.exists(paste(input_dir, dat_fils, sep = "/")))
if (fil_not_exists) {stop("Input file not found")}

# Files for merging.
print(dat_fils)
print(coord_fils)

# Initialize data frame for merge.
stim_df <- data.frame(animal = character(),
                      stimulus = integer(),
                      stim_time_sec = double(),
                      genotype = character(),
                      include = logical(),
                      time_sec = double(),
                      electrode = integer())

#Constants
sample_rate <- 100
convert_current <- TRUE

# Read data
animal_id_last_file <- 0
max_stim_last_file <- 0

for (i in 1:(length(coord_fils) - 0)) {
        print(paste0(coord_fils[i], " : ", dat_fils[i]))
        dat <- read_experiment_csv(paste(input_dir, dat_fils[i], sep = "/"),
                                   sr = sample_rate)
        coord <- fread(paste(input_dir, coord_fils[i], sep = "/"))
        coord$T_Bkg1 <- coord$T_Bkg1 * 10 # Convert Igor start times.
        
        # Routine to pick off metadata from filename
        calibration_current <- 10400
        calibration_concentration <- 5
        animal_id <- 1902052
        genotype <- "ko"

        # If new animal, reset stim counting, otherwise continue series. 
        if (animal_id != animal_id_last_file) {
                max_stim_last_file <- 0
        }
        max_stim <- nrow(coord) + max_stim_last_file
        
        if (convert_current == TRUE) {
                dat <- current_to_concentration(dat, calibration_current = calibration_current,
                                                calibration_concentration = calibration_concentration)
        }

        dat_list <- list()
        
        # j points to the coordinate row
        # j + max_stim_last_file equals the current stimulus
        for (j in 1:nrow(coord)) {
                stim <- max_stim_last_file + j
                start_idx <- coord$T_Bkg1[j]
                if (start_idx > nrow(dat)) {
                        stop(paste0("Stimulus start overflows data: ", dat_fils[j],
                                    " #", stim))
                } else if (stim == max_stim) {
                        top_row_idx <- nrow(dat)
                } else {
                        top_row_idx <- coord$T_Bkg1[(j + 1)] - 1
                }
                
                sr_s <- sample_rate * 10^-3
                
                stim_time_sec <- seq(from = 0, by = sr_s,
                                     length.out = nrow(dat[start_idx:top_row_idx, ]))
                
                one_stim_df <- cbind(animal = animal_id, stimulus = stim,
                                     stim_time_sec = stim_time_sec, genotype = genotype,
                                     include = coord$include[j],
                                     dat[start_idx:top_row_idx, ])
                
                stim_df <- rbind(stim_df, one_stim_df)
        }
        # Before leaving file loop, remember which animal we just processed.
        animal_id_last_file <- animal_id
        max_stim_last_file <- max_stim
}

# 
# # Analysis of NA
# ok <- complete.cases(stim_df)
# sum(!ok)
# 
# # WT - Pre-AMPH
# dat_merge_wt_pre <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
#         filter(genotype == "wt" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 120) %>%
#         group_by(stim_time_sec) %>%
#         summarize(mean(electrode))
# dat_merge_wt_pre <- rename(dat_merge_wt_pre, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
# qplot(dat_merge_wt_pre$time_sec, dat_merge_wt_pre$electrode, geom = "line")
# 
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
# # Constants
# pulses <- 30
# pulse_freq <- 50
# bin_size <- 2.0
# electrode_distance <- 1000
# dead_space_distance <- 4
# diffusion_coefficient <- 2.7 * 10^-6
# convert_current <- FALSE
# fit_region = "fall"
# 
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
# # KO - Pre-AMPH
# 
# # Variables
# genotype <- "KO"
# amphetamine <- "PRE"
# release <- 1.77
# vmax <- 4.8
# km <- 5
# base_tolerance <- 0.05
# plot_duration_sec = 10
# 
# if (nrow(results[results$genotype == genotype & results$amphetamine == amphetamine, ]) == 0) {
#         results[(nrow(results)+1), ] <- c(genotype, amphetamine, release, vmax, km)
# } else {
#         results[results$genotype == genotype & 
#                         results$amphetamine == amphetamine, ] <- cbind(genotype, amphetamine, release, vmax, km)
# }
# 
# compare_pulse(dat = dat_merge_ko_pre, fil = "Knockout - Pre-AMPH",
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

