library(ggplot2)
library(openxlsx)
library(dplyr)
library(data.table)

# Global variables
pipeline_dir <- "./pipeline"
input_dir <- library_dir <- paste(pipeline_dir, "06_Library", sep = "/")
coordinate_file_tag <- "PD"
data_file_tag <- "DAT"
fil_extension <- "csv"
sample_rate <- 100
stim_period <- 120 # seconds

input_queue <- dir(input_dir)
# Filter for coordinate files
pat <- paste0(coordinate_file_tag, "\\.", fil_extension, "$")
input_queue <- input_queue[grep(pattern = pat, x = input_queue, ignore.case = TRUE)]

# fils <- unique(fil_params_all[ , c("filename", "sample_rate", "animal", "genotype",
#                                    "pulses", "pulse_freq", "bin_size", "electrode_distance",
#                                    "dead_space_distance", "diffusion_coefficient", "convert_current",
#                                    "calibration_current", "calibration_concentration")])


# Files for merging.
print(input_queue)

# Initialize data frame for merge.
stim_df <- data.frame(animal = integer(),
                      stimulus = integer(),
                      stim_time_sec = double(),
                      genotype = character(),
                      include = logical(),
                      time_sec = double(),
                      electrode_current = double(),
                      electrode_concentration = double())

animal_id_last_file <- 0
max_stim_last_file <- 0

# Read data
for (i in 1:(length(input_queue) - 0)) {
        # Coordinate file drives processing.
        coord_fil <- input_queue[i]
        print(coord_fil)
        coord_fil_path <- paste(input_dir, coord_fil, sep = "/")
        
        # Derive data file name.
        dat_fil <- sub(pattern = paste0(coordinate_file_tag, "\\.", fil_extension, "$"),
                       replacement = paste0(data_file_tag, "\\.", fil_extension),
                       x = coord_fil,
                       ignore.case = TRUE)
        dat_fil_path <- paste(input_dir, dat_fil, sep = "/")
        
        #Parse file name
        file_tags <- parse_file_name(dat_fil)
        animal_id <- file_tags[[1]]
        # skip file_tags[[2]]. It is the file_type.
        genotype <- file_tags[[3]]
        drug_concentration <- file_tags[[4]]
        # drug_name <- UNSUPPORTED
        drug_position <- file_tags[[5]]
        calibration_current <- file_tags[[6]]
        calibration_concentration <- file_tags[[7]]
        
        # Read the coordinate file
        coord <- fread(coord_fil_path)
        # coord$T_Bkg1 <- coord$T_Bkg1 * 10 # Convert Igor start times.
        # coord$T_Bkg2 <- coord$T_Bkg2 * 10 # Convert Igor start times.
        
        # Read data. Electrode as current.
        dat_current <- read_experiment_csv(dat_fil_path, sr = sample_rate)
        # Read data. Electrode as concentration.
        dat_concentration <- read_experiment_csv(dat_fil_path, sr = sample_rate)
        dat_concentration <- current_to_concentration(dat_concentration,
                                                      calibration_current = calibration_current,
                                                      calibration_concentration = calibration_concentration)
        
        # If new animal, reset stim counting, otherwise continue series. 
        if (animal_id != animal_id_last_file) {
                max_stim_last_file <- 0
        }
        max_stim <- nrow(coord) + max_stim_last_file
        
        dat_current_list <- list()
        dat_concentration_list <- list()
        
        # j points to the coordinate row
        # j + max_stim_last_file equals the current stimulus
        for (j in 1:nrow(coord)) {
                stim <- max_stim_last_file + j
                stim_start_time <- coord$T_Bkg1[j]
                dat_start_idx <- which(round(dat_current$time_sec, 1) == round(coord$T_Bkg1[j], 1))
                if (dat_start_idx > nrow(dat_current)) {
                        stop(paste0("Stimulus start overflows data: ", coord_fil,
                                    " #", stim))
                } else if (stim == max_stim) {
                        top_row_idx <- nrow(dat_current)
                } else {
                        top_row_idx <- which(round(dat_current$time_sec, 1) == round(coord$T_Bkg1[(j + 1)], 1)) - 1
                }
                
                sr_s <- sample_rate * 10^-3
                
                stim_time_sec <- seq(from = 0, by = sr_s,
                                     length.out = nrow(dat_current[dat_start_idx:top_row_idx, ]))

                one_stim_df <- as.data.frame(cbind(animal = animal_id, stimulus = stim,
                                     stim_time_sec = stim_time_sec, genotype = genotype,
                                     include = coord$include[j],
                                     time_sec = dat_current[dat_start_idx:top_row_idx, 1],
                                     electrode_current = dat_current[dat_start_idx:top_row_idx, 2],
                                     electrode_concentration = dat_concentration[dat_start_idx:top_row_idx, 2]),
                                     stringsAsFactors = FALSE)
                
                one_stim_df$animal <- as.integer(one_stim_df$animal)
                one_stim_df$stimulus <- as.integer(one_stim_df$stimulus)
                one_stim_df$stim_time_sec <- as.double(one_stim_df$stim_time_sec)
                one_stim_df$include <- as.logical(one_stim_df$include)
                one_stim_df$time_sec <- as.double(one_stim_df$time_sec)
                one_stim_df$electrode_current <- as.double(one_stim_df$electrode_current)
                one_stim_df$electrode_concentration <- as.double(one_stim_df$electrode_concentration)
                
                stim_df <- rbind(stim_df, one_stim_df)
        }
        # Before leaving file loop, remember which animal we just processed.
        animal_id_last_file <- animal_id
        max_stim_last_file <- max_stim
}

# # Analysis of NA
ok <- complete.cases(stim_df)
sum(!ok)
 
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
dat_merge_ko_pre <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
        filter(genotype == "ko" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 120) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode_concentration))
dat_merge_ko_pre <- rename(dat_merge_ko_pre, time_sec = stim_time_sec, "electrode_concentration" = "mean(electrode_concentration)")
qplot(dat_merge_ko_pre$time_sec, dat_merge_ko_pre$electrode_concentration, geom = "line")

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
# 
