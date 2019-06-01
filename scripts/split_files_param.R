library(ggplot2)
library(openxlsx)

input_dir <- "./input"           # Input directory, on GitHub 
exp_dir <- "./exp/csv"           # Experiment data, not on GitHub
par_dir <- "./scripts"           # File params need a trackable directory
sample_rate <- 100               # milliseconds

# File parameters, on GitHub
fil_params_all <- read.xlsx(paste(par_dir, "file_params.xlsx", sep = "/"))

fils <- unique(fil_params_all$filename)

fil_not_exists <- sum(!file.exists(paste(input_dir, fils, sep = "/")))
if (fil_not_exists) {stop("Input file not found")}

# Pick a file to work on.
print(fils)
i <- 2
dat <- read_experiment_csv(paste(input_dir, fils[i], sep = "/"), sr = sample_rate)
# Plot the sweep. Manually save it and view.
qplot(dat$time_sec, dat$electrode, geom = "line")

# Get to work. Repeat this block
fil_params_all <- read.xlsx(paste(par_dir, "file_params.xlsx", sep = "/"))
fil_params_cur <- fil_params_all[fil_params_all$filename == fils[i] , c("stimulus", "start")]
dat_list <- list()
max_stim <- max(fil_params_cur$stimulus)
for (stim in fil_params_cur$stimulus) {
        start_idx <- fil_params_cur[stim, "start"]
        if (stim == max_stim) {
                top_row_idx <- nrow(dat)
        } else {
                top_row_idx <- fil_params_cur[(stim + 1), "start"] - 1
        }
        
        dat_list[[stim]] <- dat[start_idx:top_row_idx, ]
}
for (j in 1:(length(dat_list) - 0)) {
        p <- qplot(dat_list[[j]]$time_sec, dat_list[[j]]$electrode, geom = "line",
                   main = paste0(fils[i], "_", j))
        print(p)
}
