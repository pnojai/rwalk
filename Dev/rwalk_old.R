rwalk_tut <- function(bins = 7, iters = 5, mols = 7000, smooth = 4) {
        # Initialize a matrix. Give it an extra row for time = 0,
        # and an extra column for smoothing the reflecting surface.
        rw <- matrix(rep(0, (bins + 1) * (iters + 1)), iters + 1, bins + 1)
        
        # Release molecules from the reflecting surface at time 0 (row 1)
        rw[1, 1] <- mols
        
        # Iterate in time
        for (i in 2:(iters + 1)) {
                rw[i, 1] <- mean(rw[(i - 1), 1:2])
                
                # Diffuse the molecules until you're two away from the electrode.
                for (j in 2:(bins - 2)) {
                        rw[i, j] <- mean(c(rw[i - 1, j - 1], rw[i - 1, j + 1]))
                }
                
                # Diffuse the molecules next to the electrode.
                rw[i, bins -1] <- .5 * rw[i - 1, bins - 2]
                
                # And at the electrode.
                rw[i, bins] <- .5 * rw[i - 1, bins - 1]
                
        }
        
        # Smooth the counts at the electrode.
        for (i in 1:(iters + 1 - smooth + 1)) {
                rw[i, bins + 1] <- mean(rw[i:(i + smooth - 1), bins])
        }
        
        #Return the random walk matrix.
        rw
}

rwalk_amp <- function(vmax = 4.57, km = .78, release = 2.75, bin_size = 2.0,
                      electrode_distance = 50.0 , dead_space_distance = 4.0,
                      diffusion_coefficient = .0000027, duration = 1) {
        # Amperometry simulation
        # Author: Jai Jeffryes
        # Email: jai@jeffryes.net
        
        # Parameters
        # vmax: Micro M / sec.
        # km: Micro M.
        # release: Micro M.
        # bin_size: Micrometres.
        # bin_number_left: Number of bins left of the electrode.
        # bin_number_right: Number of bins right of the electrode.
        # diffusion_coefficient: square centimeters / second.
        # duration: span of diffusion in seconds.
        
        # Calculate the duration of an iteration.
        it_dur <- iteration_duration(diffusion_coefficient = diffusion_coefficient, bin_size = bin_size)
        
        iterations <- as.integer(duration / it_dur)
        
        # Calculate bin number
        bin_number_displace <- as.integer(electrode_distance / bin_size)
        
        # Initialize a matrix. Give it an extra row for time = 0.
        # Bins = specified columns to the left of the electrode, to the right, and electrode in the middle.
        bins <- 2 * bin_number_displace  + 1
        rw <- matrix(rep(0.0, (bins) * (iterations + 1)), iterations + 1, bins)
        
        time_sec <- seq(from = 0, by = it_dur, length.out = nrow(rw))
        
        # Position the electrode and the dead space
        electrode_pos <- electrode_pos(rw, time_column = FALSE) # No time series on the front yet.
        
        # Identify dead spaces in a logical vector
        dead_space_displace <- as.integer(dead_space_distance / bin_size)
        dead_space_range <- (bin_number_displace - dead_space_displace + 1):(bin_number_displace + dead_space_displace + 1)
        dead_space_bin <- rep(FALSE, bins)
        dead_space_bin[dead_space_range] <- TRUE
        
        # Release at time 0 (row 1). Don't release to dead space.
        rw[1, !dead_space_bin] <- release
        
        # Iterate in time
        for (i in 2:(iterations + 1)) {
                # Fill extreme bins.
                # Outside bins take from inside neighbor only
                curr_bin <- 1
                inside_neighbor <- curr_bin + 1
                val <- mean(c(rw[(i - 1), inside_neighbor], 0))
                val <- micmen(val, vmax, km, it_dur)
                rw[i, 1] <- val
                
                #Really, could assume this is the same, but hey, I'm cautious
                mirror_bin <- bins
                inside_neighbor <- mirror_bin - 1
                val <- mean(c(rw[(i - 1), inside_neighbor], 0))
                val <- micmen(val, vmax, km, it_dur)
                rw[i, mirror_bin] <-  val
                
                # 2nd bins in take .711 from outside neighbor, .5 from inside
                curr_bin <- 2
                inside_neighbor <- curr_bin + 1
                outside_neighbor <- curr_bin - 1
                val <- .711 * rw[(i - 1), outside_neighbor] + .5 * rw[(i - 1), inside_neighbor]
                val <- micmen(val, vmax, km, it_dur)
                rw[i, curr_bin] <- val
                
                # Same, but cautious
                mirror_bin <- bins - 1
                inside_neighbor <- mirror_bin - 1
                outside_neighbor <- mirror_bin + 1
                val <- .711 * rw[(i - 1), outside_neighbor] + .5 * rw[(i - 1), inside_neighbor]
                val <- micmen(val, vmax, km, it_dur)
                rw[i, mirror_bin] <- val
                
                # Diffuse the molecules until you're one away from the electrode.
                # Think about it like you're working inwards along the displacements from the electrode.
                for (j in 3:(electrode_pos - 2)) {
                        outside_neighbor <- j - 1
                        inside_neighbor <- j + 1
                        
                        val <- mean(c(rw[i - 1, outside_neighbor], rw[i - 1, inside_neighbor]))
                        val <- micmen(val, vmax, km, it_dur)
                        rw[i, j] <- val
                        
                        # Do it at the mirror bin. Don't fry your brain on the indexes.
                        mirror_bin <- bins - j + 1
                        outside_neighbor <- mirror_bin + 1
                        inside_neighbor <- mirror_bin - 1
                        
                        val <- mean(c(rw[i - 1, outside_neighbor], rw[i - 1, inside_neighbor]))
                        val <- micmen(val, vmax, km, it_dur)
                        rw[i, mirror_bin] <- val
                        
                }
                
                # Diffuse the molecules next to the electrode. Receives .5 from outside.
                curr_bin <- electrode_pos - 1
                outside_neighbor <- curr_bin - 1
                
                val <- .5 * rw[i - 1, outside_neighbor]
                val <- micmen(val, vmax, km, it_dur)
                rw[i, curr_bin] <- val
                
                mirror_bin <- electrode_pos + 1
                outside_neighbor <- curr_bin - 1
                
                val <- .5 * rw[i - 1, outside_neighbor]
                val <- micmen(val, vmax, km, it_dur)
                rw[i, mirror_bin] <- val
                
                # And at the electrode.
                val <- .5 * rw[i - 1, electrode_pos - 1] + .5 * rw[i - 1, electrode_pos + 1]
                # Note: there is no Michaelis-Menten correction for uptake at the electrode.
                rw[i, electrode_pos] <- val
                
        }
        
        # Add the time series to the front of the data frame.
        rw_df <- as.data.frame(cbind(time_sec, rw))
        # Name the location of the electrode data.
        colnames(rw_df)[electrode_pos(rw_df, time_column = TRUE)] <- "electrode"
        
        rw_df
        
}

rwalk_cv <- function(vmax = 4.57, km = .78, release = 2.75, bin_size = 2.0,
                     electrode_distance = 50.0 , dead_space_distance = 4.0,
                     diffusion_coefficient = .0000027, duration = 1) {
        # Amperometry simulation
        # Author: Jai Jeffryes
        # Email: jai@jeffryes.net
        
        # Parameters
        # vmax: Micro M / sec.
        # km: Micro M.
        # release: Micro M.
        # bin_size: Micrometres.
        # bin_number_left: Number of bins left of the electrode.
        # bin_number_right: Number of bins right of the electrode.
        # diffusion_coefficient: square centimeters / second.
        # duration: span of diffusion in seconds.
        
        # Calculate the duration of an iteration.
        it_dur <- iteration_duration(diffusion_coefficient = diffusion_coefficient, bin_size = bin_size)
        
        iterations <- as.integer(duration / it_dur)
        
        # Calculate bin number
        bin_number_displace <- as.integer(electrode_distance / bin_size)
        
        # Initialize a matrix. Give it an extra row for time = 0.
        # Bins = specified columns to the left of the electrode, to the right, and electrode in the middle.
        bins <- 2 * bin_number_displace  + 1
        rw <- matrix(rep(0.0, (bins) * (iterations + 1)), iterations + 1, bins)
        
        time_sec <- seq(from = 0, by = it_dur, length.out = nrow(rw))
        
        # Position the electrode and the dead space
        electrode_pos <- electrode_pos(rw, time_column = FALSE) # No time series on the front yet.
        
        # Identify dead spaces in a logical vector
        dead_space_displace <- as.integer(dead_space_distance / bin_size)
        dead_space_range <- (bin_number_displace - dead_space_displace + 1):(bin_number_displace + dead_space_displace + 1)
        dead_space_bin <- rep(FALSE, bins)
        dead_space_bin[dead_space_range] <- TRUE
        
        # Release at time 0 (row 1). Don't release to dead space.
        rw[1, !dead_space_bin] <- release
        
        # Iterate in time
        for (i in 2:(iterations + 1)) {
                # Fill extreme bins.
                # Outside bins take from inside neighbor only
                curr_bin <- 1
                inside_neighbor <- curr_bin + 1
                val <- mean(c(rw[(i - 1), inside_neighbor], 0))
                val <- micmen(val, vmax, km, it_dur)
                rw[i, 1] <- val
                
                #Really, could assume this is the same, but hey, I'm cautious
                mirror_bin <- bins
                inside_neighbor <- mirror_bin - 1
                val <- mean(c(rw[(i - 1), inside_neighbor], 0))
                val <- micmen(val, vmax, km, it_dur)
                rw[i, mirror_bin] <-  val
                
                # 2nd bins in take .711 from outside neighbor, .5 from inside
                curr_bin <- 2
                inside_neighbor <- curr_bin + 1
                outside_neighbor <- curr_bin - 1
                val <- .711 * rw[(i - 1), outside_neighbor] + .5 * rw[(i - 1), inside_neighbor]
                val <- micmen(val, vmax, km, it_dur)
                rw[i, curr_bin] <- val
                
                # Same, but cautious
                mirror_bin <- bins - 1
                inside_neighbor <- mirror_bin - 1
                outside_neighbor <- mirror_bin + 1
                val <- .711 * rw[(i - 1), outside_neighbor] + .5 * rw[(i - 1), inside_neighbor]
                val <- micmen(val, vmax, km, it_dur)
                rw[i, mirror_bin] <- val
                
                # Diffuse the molecules until you get to the electrode.
                # Think about it like you're working inwards along the displacements from the electrode.
                for (j in 3:(electrode_pos - 1)) { # Only difference from the amperometry simulation.
                        outside_neighbor <- j - 1
                        inside_neighbor <- j + 1
                        
                        val <- mean(c(rw[i - 1, outside_neighbor], rw[i - 1, inside_neighbor]))
                        val <- micmen(val, vmax, km, it_dur)
                        rw[i, j] <- val
                        
                        # Do it at the mirror bin. Don't fry your brain on the indexes.
                        mirror_bin <- bins - j + 1
                        outside_neighbor <- mirror_bin + 1
                        inside_neighbor <- mirror_bin - 1
                        
                        val <- mean(c(rw[i - 1, outside_neighbor], rw[i - 1, inside_neighbor]))
                        val <- micmen(val, vmax, km, it_dur)
                        rw[i, mirror_bin] <- val
                        
                }
                
                # Diffuse the molecules at the electrode.
                val <- .5 * rw[i - 1, electrode_pos - 1] + .5 * rw[i - 1, electrode_pos + 1]
                # Note: there is no Michaelis-Menten correction for uptake at the electrode.
                rw[i, electrode_pos] <- val
                
        }
        
        # Add the time series to the front of the data frame.
        rw_df <- as.data.frame(cbind(time_sec, rw))
        # Name the location of the electrode data.
        colnames(rw_df)[electrode_pos(rw_df, time_column = TRUE)] <- "electrode"
        
        rw_df
}

rwalk_cv_pulse_before_reduced <- function(vmax, km, release, pulses,
                                          pulse_freq, bin_size, electrode_distance,
                                          dead_space_distance, diffusion_coefficient,
                                          duration) {
        # Amperometry simulation
        # Author: Jai Jeffryes
        # Email: jai@jeffryes.net
        
        # Parameters
        # vmax: Micro M / sec.
        # km: Micro M.
        # release: Micro M.
        # bin_size: Micrometres.
        # bin_number_left: Number of bins left of the electrode.
        # bin_number_right: Number of bins right of the electrode.
        # diffusion_coefficient: square centimeters / second.
        # duration: span of diffusion in seconds.
        
        # Calculate the duration of an iteration.
        it_dur <- iteration_duration(diffusion_coefficient = diffusion_coefficient, bin_size = bin_size)
        
        iterations <- as.integer(duration / it_dur)
        
        # Calculate bin number
        bin_number_displace <- as.integer(electrode_distance / bin_size)
        
        # Initialize a matrix. Give it an extra row for time = 0.
        # Bins = specified columns to the left of the electrode, to the right, and electrode in the middle.
        bins <- 2 * bin_number_displace  + 1
        rw <- matrix(rep(0.0, (bins) * (iterations + 1)), iterations + 1, bins)
        
        time_sec <- seq(from = 0, by = it_dur, length.out = nrow(rw))
        
        # Calculate releases and assign them to time stamps.
        # Prorate the release across all pulses.
        release_timed <- release / pulses
        # Time between pulses.
        pulse_dur <- 1.0 / pulse_freq
        # Times for releases.
        releases <- 0:(pulses - 1) * pulse_dur
        # Closest timestamps in matrix for releases.
        release_time_sec_idx <- sapply(releases, function(x) {which.min(abs(time_sec - x))})
        
        # Position the electrode and the dead space
        electrode_pos <- electrode_pos(rw, time_column = FALSE) # No time series on the front yet.
        
        # Identify dead spaces in a logical vector
        dead_space_displace <- as.integer(dead_space_distance / bin_size)
        dead_space_range <- (bin_number_displace - dead_space_displace + 1):(bin_number_displace + dead_space_displace + 1)
        dead_space_bin <- rep(FALSE, bins)
        dead_space_bin[dead_space_range] <- TRUE
        
        # Release at time 0 (row 1). Don't release to dead space.
        # rw[1, !dead_space_bin] <- release
        rw[1, !dead_space_bin] <- release_timed
        
        # Iterate in time
        for (i in 2:(iterations + 1)) {
                # Fill extreme bins.
                # Outside bins take from inside neighbor only
                curr_bin <- 1
                inside_neighbor <- curr_bin + 1
                val <- mean(c(rw[(i - 1), inside_neighbor], 0))
                val <- micmen(val, vmax, km, it_dur)
                rw[i, 1] <- val
                if (i %in% release_time_sec_idx & !dead_space_bin[1]) {
                        #print(paste("Releasing in time index:", i))
                        rw[i, 1] <- rw[i, 1] + release_timed
                }
                
                # Mirror bin is identical.
                mirror_bin <- bins
                # inside_neighbor <- mirror_bin - 1
                rw[i, mirror_bin] <- rw[i, 1]
                
                # 2nd bins in take .711 from outside neighbor, .5 from inside
                curr_bin <- 2
                inside_neighbor <- curr_bin + 1
                outside_neighbor <- curr_bin - 1
                val <- .711 * rw[(i - 1), outside_neighbor] + .5 * rw[(i - 1), inside_neighbor]
                val <- micmen(val, vmax, km, it_dur)
                rw[i, curr_bin] <- val
                if (i %in% release_time_sec_idx & !dead_space_bin[curr_bin]) {
                        #print(paste("Releasing in time index:", i))
                        rw[i, curr_bin] <- rw[i, curr_bin] + release_timed
                }
                
                # Same.
                mirror_bin <- bins - 1
                # inside_neighbor <- mirror_bin - 1
                # outside_neighbor <- mirror_bin + 1
                # val <- .711 * rw[(i - 1), outside_neighbor] + .5 * rw[(i - 1), inside_neighbor]
                # val <- micmen(val, vmax, km, it_dur)
                # rw[i, mirror_bin] <- val
                # if (i %in% release_time_sec_idx & !dead_space_bin[mirror_bin]) {
                #         #print(paste("Releasing in time index:", i))
                #         rw[i, curr_bin] <- rw[i, curr_bin] + release_timed # That was a bug, I think.
                #                       Should have been mirror_bin, not curr_bin.
                # }
                rw[i, mirror_bin] <- rw[i, curr_bin]
                
                # Diffuse the molecules until you get to the electrode.
                # Think about it like you're working inwards along the displacements from the electrode.
                for (j in 3:(electrode_pos - 1)) { # Only difference from the amperometry simulation.
                        outside_neighbor <- j - 1
                        inside_neighbor <- j + 1
                        
                        val <- mean(c(rw[i - 1, outside_neighbor], rw[i - 1, inside_neighbor]))
                        val <- micmen(val, vmax, km, it_dur)
                        rw[i, j] <- val
                        if (i %in% release_time_sec_idx & !dead_space_bin[j]) {
                                #print(paste("Releasing in time index:", i))
                                rw[i, j] <- rw[i, j] + release_timed
                        }
                        
                        # Do it at the mirror bin. Don't fry your brain on the indexes.
                        mirror_bin <- bins - j + 1
                        
                        # outside_neighbor <- mirror_bin + 1
                        # inside_neighbor <- mirror_bin - 1
                        # val <- mean(c(rw[i - 1, outside_neighbor], rw[i - 1, inside_neighbor]))
                        # val <- micmen(val, vmax, km, it_dur)
                        # rw[i, mirror_bin] <- val
                        # if (i %in% release_time_sec_idx & !dead_space_bin[mirror_bin]) {
                        #         #print(paste("Releasing in time index:", i))
                        #         rw[i, mirror_bin] <- rw[i, mirror_bin] + release_timed
                        # }
                        rw[i, mirror_bin] <- rw[i, j]
                        
                }
                
                # Diffuse the molecules at the electrode.
                val <- .5 * rw[i - 1, electrode_pos - 1] + .5 * rw[i - 1, electrode_pos + 1]
                # Note: there is no Michaelis-Menten correction for uptake at the electrode.
                rw[i, electrode_pos] <- val
                if (i %in% release_time_sec_idx & !dead_space_bin[electrode_pos]) {
                        #print(paste("Releasing in time index:", i))
                        rw[i, electrode_pos] <- rw[i, electrode_pos] + release_timed
                }
                
        }
        
        # Add the time series to the front of the data frame.
        rw_df <- as.data.frame(cbind(time_sec, rw))
        # Name the location of the electrode data.
        colnames(rw_df)[electrode_pos(rw_df, time_column = TRUE)] <- "electrode"
        
        rw_df
}

compare <- function(fil, sample_rate = 100, vmax = 4.57, km = .78, release = 2.75,
                    bin_size = .5, electrode_distance = 50.0, dead_space_distance = 4.0,
                    diffusion_coefficient = .0000027, smoothing_count = 4,
                    convert_current = TRUE, calibration_current = NULL,
                    calibration_concentration = NULL) {
        
        # Read data file.
        print("Reading data...")
        dat <- read_experiment_csv(fil, sr = sample_rate)
        
        if (convert_current) {
                dat <- current_to_concentration(dat, calibration_current, calibration_concentration)
        }
        
        # Set plotting parameters for simulation
        # Domain
        # Find the time of the first observation before the stimulus. Assume the stimulus begins at the
        # most jumpy data point. Find the maximum second derivative and take the index right before it.
        
        print("Setting up parameters of plot...")
        max_obs <- max(dat$electrode)
        idx_max_obs <- which(dat$electrode == max_obs) # Index of peak
        
        # Get the index where the stimulus starts.
        idx_stim_start <- get_stim_start(dat[1:idx_max_obs, ])
        
        # Get the minimum observation in the 1st partition. Find the index.
        # REPLACED BY IDX_STIM_START
        # idx_min_obs <- which(dat$electrode == min(dat[1:idx_max_obs, 2]))
        # idx_min_obs <- idx_min_obs[idx_min_obs < idx_max_obs] # Min obs earlier than peak.
        
        # min_time <- dat[idx_min_obs, "time_sec"]
        min_time <- dat[idx_stim_start, "time_sec"]
        max_time <- max(dat$time_sec)
        
        # Range
        # Don't adjust the baseline. Y starts wherever the data begins.
        # y_base <- dat[idx_min_obs, 2]
        
        # Duration of simulation.
        dur <- max_time - min_time
        
        is_debug <- FALSE
        if (is_debug) {
                print(paste("max_obs:", max_obs))
                print(paste("idx_max_obs:", idx_max_obs))
                # print(paste("idx_min_obs:", idx_min_obs))
                print(paste("min_time:", min_time))
                print(paste("max_time:", max_time))
                # print(paste("y_base:", y_base))
                print(paste("dur:", dur))
                
                write.csv(dat[1:idx_max_obs, ], "Data/debug_datToMax")
        }
        
        # Calculate random walk.
        print("Building random walk...")
        rw <- rwalk_cv(vmax = vmax, km = km, release = release, bin_size = bin_size,
                       electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
                       diffusion_coefficient = diffusion_coefficient, duration = dur)
        
        print("Formatting results...")
        # Pick off the results at the simulated electrode.
        sim <- electrode_results(rw, electrode_pos = electrode_pos(rw), smoothing_count = 4)
        
        # Shift the time of the results.
        sim$time_sec <- sim$time_sec + min_time
        
        # Make a tall data set.
        sim_w_src <- cbind(sim, src = "simulation")
        # Don't adjust the baseline. Y starts wherever the data begins.
        # sim_w_src$electrode <- sim$electrode + y_base
        
        dat_w_src <- cbind(dat, src = "experiment")
        
        sim_w_dat <- rbind(sim_w_src, dat_w_src)
        
        # write.csv(sim_w_dat[sim_w_dat$time_sec >= min_time, ], file = "Data/compare.csv")
        
        # Correlate the simulation and the experimental data.
        # Need an equal number of points on each side. Up sample the experimental data
        # based on the time series in the simulation.
        
        dat_slp_intcpt <- slope_intercept_df(dat_w_src[ , 1:2])
        
        # Build a new data frame for up sampled experimental data.
        # Time series from the model.
        #dat_up <- cbind(sim_w_src$time_sec)
        dat_up <- get_slope_intercepts(dat_slp_intcpt, sim_w_src$time_sec)
        
        # Algebra for interpolation
        m <- dat_up$slope
        x <- dat_up$time_sec
        b <- dat_up$intercept
        
        interpolate <- m * x + b
        
        dat_up <- cbind(dat_up, electrode = interpolate)
        dat_up <- cbind(dat_up, src = "interpolation", stringsAsFactors = FALSE)
        
        sim_w_datup <- rbind(sim_w_src, dat_up[ , c(1, 4, 5)])
        
        # Correlate simulation and up sampled data
        r2 <- rsq(sim_w_src[, 2], dat_up[ , 4])
        
        # Superimpose the plots
        # Greek letters. Here's how to include them in labels.
        # https://stats.idre.ucla.edu/r/codefragments/greek_letters/
        print("Ready to plot.")
        
        plot_rwalk(sim_w_dat, fil, release, vmax, km, r2, calibration_current = calibration_current,
                   calibration_concentration = calibration_concentration)
        
        # caption <- paste("release=", release, "\n", "vmax=", vmax, "\n", "km=", km, "\n",
        #                  "r2=", round(r2, 6), sep = "")
        # 
        # ggplot(data = sim_w_dat) +
        #         geom_line(mapping = aes(x = time_sec, y = electrode, colour = src)) +
        #         labs(title = "Cyclic Voltammetry Simulation",
        #                 subtitle = paste("Input Data File: ", fil),
        #              x = "time [s]",
        #              y = expression(paste("DA concentration [", mu, "M]")),
        #              colour = "source") +
        #         annotate("text", x = Inf, y = Inf, label = caption, vjust = 1, hjust = 1)
        
        # ggplot(data = sim_w_datup) +
        #         geom_line(mapping = aes(x = time_sec, y = electrode, colour = src)) +
        #         labs(title = "Cyclic Voltammetry Simulation",
        #              subtitle = paste("Input Data File: ", fil),
        #              x = "time [s]",
        #              y = expression(paste("DA concentration [", mu, "M]")),
        #              colour = "source") +
        #         annotate("text", x = Inf, y = Inf, label = caption, vjust = 1, hjust = 1)
}

trim_results <- function(df) {
        # Find peak. We'll start there.
        start_idx <- which.max(df[ ,1 ])
        end_idx <- nrow(df)
        print(start_idx)
        print(end_idx)
        
        df_trim <- as.data.frame(df[start_idx:end_idx, 1])
        row.names(df_trim) <- row.names(df)[start_idx:end_idx]
        
        df_trim
}

