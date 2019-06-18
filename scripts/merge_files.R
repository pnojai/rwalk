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
for (i in 1:(nrow(fils) - 0)) {
        print(fils[i, "filename"])
        
        dat <- read_experiment_csv(paste(input_dir, fils[i, "filename"], sep = "/"),
                                   sr = fils[i, "sample_rate"])
        
        if (fils[i, "convert_current"] == TRUE) {
                dat <- current_to_concentration(dat, calibration_current = fils[i, "calibration_current"],
                                                calibration_concentration = fils[i, "calibration_concentration"])
        }
        
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

dat_merge <- select(stim_df, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 1 & stimulus <= 3 & include == TRUE) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode))

# dat_merge <- select(stim_df, stim_time_sec, electrode, genotype, stimulus, include) %>%
#         filter(genotype == "wt" ) %>%
#         group_by(stim_time_sec) %>%
#         summarize(mean(electrode))

dat_merge <- rename(dat_merge, time_sec = stim_time_sec, "electrode" = "mean(electrode)")
qplot(dat_merge$time_sec, dat_merge$electrode, geom = "line")

release <- 2.2
vmax <- 4.8
km <- 6.5
pulses <- 30
pulse_freq <- 50
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current <- FALSE
fit_region = "fall"
base_tolerance <- 0.05
plot_duration_sec = 50

compare_pulse(dat = dat_merge, fil = "figure out a title",
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

# Analysis of NA and multiple peaks
ok <- complete.cases(stim_df)
sum(!ok)
head(stim_df[!ok,])

unique(paste(stim_df$animal[!ok], stim_df$stimulus[!ok]))

head(stim_df)
tail(stim_df)
