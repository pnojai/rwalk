calculate_time_correction <- function(coord, dat_fn, sample_rate, stim_period) {
        tolerance <- .001
        
        dat <- read_experiment_csv(dat_fn, sr = sample_rate)
        
        igor_time_max_stim_1 <- as.numeric(coord[1, 1])
        igor_start_stim_1 <- as.numeric(coord[1, 2])
        time_cutoff_stim_1 <- igor_start_stim_1 + stim_period
        
        dat_stim_1 <- dat[dat$time_sec <= time_cutoff_stim_1, ]
        
        electrode_max <- max(dat_stim_1[dat_stim_1$time_sec <= time_cutoff_stim_1, "electrode"])
        actual_time_max_stim_1 <- dat_stim_1[dat_stim_1$electrode == electrode_max, "time_sec"]
        
        time_correction <- actual_time_max_stim_1 - igor_time_max_stim_1
        
        if (time_correction < tolerance) {
                result <- 0
        } else {
                result <- time_correction
        }
        
        result
}

preprocess_coord_file <- function(coord_fn, dat_fn, input_dir, output_dir, sample_rate,
                                  stim_period) {
        
        # Split coordinate file name.
        
        # Read the coordinate file
        coord <- data.table::fread(paste(input_dir, coord_fn, sep = "/"))

        # Igor times sometimes are displaced due to a bug.
        # Correct them.
        time_correction <- calculate_time_correction(coord = coord, dat_fn = dat_fn,
                                                     sample_rate = sample_rate,
                                                     stim_period = stim_period)
        
        if (time_correction != 0) {
                coord <- coord + time_correction
        }
        
        # Append column for toggling inclusion of a stimulus
        coord <- cbind(coord, include = TRUE)
        
        print(head(coord))
        
        1
        
}