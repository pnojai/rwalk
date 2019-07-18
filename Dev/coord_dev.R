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


# Walk through an time adjustment.
# Parameters will be filenames. Return an adjusted dataframe for the coordinate file.
library(data.table)
sample_rate <- 100
stim_period <- 120 # seconds
coord_fn <- "1902052_a-synKO_AMPH4_CURR10400_CONC5_FILE1_peakdetection.csv"
dat_fn <- "1902052_a-synKO_AMPH4_CURR10400_CONC5_FILE1.csv"

coord <- fread(paste(input_dir, coord_fn, sep = "/"))
dat <- read_experiment_csv(paste(input_dir, dat_fn, sep = "/"), sr = sample_rate)

igor_time_max_stim_1 <- as.numeric(coord[1, 1])
igor_start_stim_1 <- as.numeric(coord[1, 2])
time_cutoff_stim_1 <- igor_start_stim_1 + stim_period

dat_stim_1 <- dat[dat$time_sec <= time_cutoff_stim_1, ]

electrode_max <- max(dat_stim_1[dat_stim_1$time_sec <= time_cutoff_stim_1, "electrode"])
actual_time_max_stim_1 <- dat_stim_1[dat_stim_1$electrode == electrode_max, "time_sec"]

time_correction <- actual_time_max_stim_1 - igor_time_max_stim_1
