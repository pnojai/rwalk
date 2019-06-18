library(ggplot2)
library(openxlsx)

input_dir <- "./input"           # Input directory, on GitHub
par_dir <- "./scripts"           # File params need a trackable directory

# File parameters, on GitHub
fil_params_all <- read.xlsx(paste(par_dir, "file_params.xlsx", sep = "/"))

fils <- unique(fil_params_all$filename)

fil_not_exists <- sum(!file.exists(paste(input_dir, fils, sep = "/")))
if (fil_not_exists) {stop("Input file not found")}

# Pick a file to work on.
print(fils)
i <- 33

fil_params_cur <- fil_params_all[fil_params_all$filename == fils[i], ]
sample_rate <- head(fil_params_cur$sample_rate, 1) # milliseconds
dat <- read_experiment_csv(paste(input_dir, fils[i], sep = "/"), sr = sample_rate)
# Plot the sweep. Manually save it and view.
qplot(dat$time_sec, dat$electrode, geom = "line")

# Get to work. Repeat this block
fil_params_all <- read.xlsx(paste(par_dir, "file_params.xlsx", sep = "/"))
fil_params_cur <- fil_params_all[fil_params_all$filename == fils[i], ]
dat_list <- list()
max_stim <- max(fil_params_cur$stimulus)

for (j in seq_along(fil_params_cur$stimulus)) {

        start_idx <- fil_params_cur$start[j] #with(fil_params_cur, start[stimulus == stim])
        if (start_idx > nrow(dat)) {
                stop("Stimulus start overflows data")
        } else if (fil_params_cur$stimulus[j] == max_stim) {
                top_row_idx <- nrow(dat)
        } else {
                top_row_idx <- fil_params_cur$start[(j + 1)] -1 # , "start"] - 1 # with(fil_params_cur, start[stimulus == stim + 1]) # fil_params_cur[(stim + 1), "start"] - 1
        }
        
        dat_list[[j]] <- dat[start_idx:top_row_idx, ]
}
for (k in 1:(length(dat_list) - 0)) {
        compare_pulse(dat = dat_list[[k]], fil = paste0(fils[i], "_", fil_params_cur[k, "stimulus"]),
                      vmax = fil_params_cur[k, "vmax"], km = fil_params_cur[k, "km"],
                      pulses = fil_params_cur[k, "pulses"],
                      pulse_freq = fil_params_cur[k, "pulse_freq"],
                      release = fil_params_cur[k, "release"],
                      bin_size = fil_params_cur[k, "bin_size"],
                      electrode_distance = fil_params_cur[k, "electrode_distance"],
                      dead_space_distance = fil_params_cur[k, "dead_space_distance"],
                      diffusion_coefficient = fil_params_cur[k, "diffusion_coefficient"],
                      convert_current = fil_params_cur[k, "convert_current"],
                      calibration_current = fil_params_cur[k, "calibration_current"],
                      calibration_concentration = fil_params_cur[k, "calibration_concentration"],
                      fit_region = fil_params_cur[k, "fit_region"],
                      base_tolerance = fil_params_cur[k, "base_tolerance"],
                      plot_duration_sec = fil_params_cur[k, "plot_duration_sec"])
}

