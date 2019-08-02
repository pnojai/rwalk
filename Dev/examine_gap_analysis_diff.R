library(ggplot2)
library(openxlsx)
library(dplyr)

input_dir <- "./input"           # Input directory, on GitHub
par_dir <- "./scripts"           # File params need a trackable directory

# File parameters, on GitHub
fil_params_all <- read.xlsx(paste(par_dir, "file_params.xlsx", sep = "/"))

fils <- unique(fil_params_all[ , c("filename", "sample_rate", "animal", "genotype",
                                   "pulses", "pulse_freq", "bin_size", "electrode_distance",
                                   "dead_space_distance", "diffusion_coefficient", "convert_current",
                                   "calibration_current", "calibration_concentration")])

fil_not_exists <- sum(!file.exists(paste(input_dir, fils$filename, sep = "/")))
if (fil_not_exists) {stop("Input file not found")}

# Files for merging.
print(fils$filename)

# Initialize data frame for merge.
stim_df <- data.frame(animal = character(),
                      stimulus = integer(),
                      stim_time_sec = double(),
                      genotype = character(),
                      include = logical(),
                      time_sec = double(),
                      electrode = integer())

# Read data
for (i in 6:6) {
        print(fils[i, "filename"])
        
        dat <- read_experiment_csv(paste(input_dir, fils[i, "filename"], sep = "/"),
                                   sr = fils[i, "sample_rate"])
        
        # if (fils[i, "convert_current"] == TRUE) {
        #         dat <- current_to_concentration(dat, calibration_current = fils[i, "calibration_current"],
        #                                         calibration_concentration = fils[i, "calibration_concentration"])
        # }
        
        fil_params_cur <- fil_params_all[fil_params_all$filename == fils[i, "filename"], ]
        # dat_list <- list()
        max_stim <- max(fil_params_cur$stimulus)
        
        for (j in seq_along(fil_params_cur$stimulus)) {
                start_idx <- fil_params_cur$start[j] # start_idx <- fil_params_cur[fil_params_cur$stimulus == stim, "start"]
                if (start_idx > nrow(dat)) {
                        stop(paste0("Stimulus start overflows data: ", fil_params_cur$filename[j],
                                    " #", fil_params_cur$stimulus[j]))
                        
                } else if (fil_params_cur$stimulus[j] == max_stim) {
                        top_row_idx <- nrow(dat)
                } else {
                        top_row_idx <- fil_params_cur$start[(j + 1)] -1 # , "start"] - 1 # fil_params_cur[fil_params_cur$stimulus == (stim + 1), "start"] - 1
                }
                
                #dat_list[[stim]] <- dat[start_idx:top_row_idx, ] # Don't really need the list
                
                sr_s <- fil_params_cur$sample_rate * 10^-3
                
                stim_time_sec <- seq(from = 0, by = sr_s,
                                     length.out = nrow(dat[start_idx:top_row_idx, ]))
                
                one_stim_df <- cbind(animal = fils[i, "animal"], stimulus = fil_params_cur$stimulus[j],
                                     stim_time_sec = stim_time_sec, genotype = fils[i, "genotype"],
                                     include = fil_params_cur$include[j], #fil_params_cur[fil_params_cur$stimulus == stim, "include"],
                                     dat[start_idx:top_row_idx, ])
                
                stim_df <- rbind(stim_df, one_stim_df)
        }
}

stim_29 <- select(stim_df, animal, stim_time_sec, time_sec, electrode, genotype, stimulus, include) %>%
        filter(animal == 1902052 & stimulus == 29)

select(stim_df, animal, stim_time_sec, time_sec, electrode, genotype, stimulus, include) %>%
        filter(animal == 1902052 & stimulus == 29 & electrode == 160181)

select(stim_df, animal, stim_time_sec, time_sec, electrode, stimulus) %>%
        filter(animal == 1902052 & stimulus == 29) %>%
        summarise(max(electrode))

# dat_merge_wt_pre <- rename(dat_merge_wt_pre, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
# qplot(dat_merge_wt_pre$time_sec, dat_merge_wt_pre$electrode, geom = "line")