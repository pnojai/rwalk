micmen <- function(x, vmax = 4.57, km = .78, duration) {
        # Correct for uptake according to the Michaelis-Menten equation.
        x - ((( vmax * x ) / ( km + x )) * duration )
        
}

# Test micmen()
# Inputs
# x: 1.375
# vmax: 4.57 (default)
# km: .78 (default)
# duration: .007407
# Expected result: 1.353
micmen(x = 1.375, duration = .007407)

iteration_duration <- function(diffusion_coefficient, bin_size) {
        # diffusion_coefficient: square centimeters / second.
        # bin_size: micrometres.
        # t = x^2/2D
        
        ((bin_size / 10000.0)^2) / (2 * diffusion_coefficient)
}

rwalk_cv_pulse <- function(vmax, km, release, pulses,
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

electrode_pos <- function(rw, time_column = TRUE) {
        # Electrode is the middle bin.
        if (time_column == TRUE) {
                pos <- (ncol(rw) / 2) + 1
        } else {
                pos <- ((ncol(rw) - 1)/ 2) + 1
        }
        
        pos
}

electrode_results <- function(rwalk_df, electrode_pos, smoothing_count = 4) {
        results <- as.data.frame(
                cbind(time_sec = rwalk_df[-1, "time_sec"],
                      electrode = rwalk_df[-1, "electrode"]
                      )
                )

        #Vector of lower indexes for means.
        seq_low <- 1:nrow(results)
        # Set up high sequence. The top boundary doesn't overflow.
        seq_high <- smoothing_count:(nrow(results) + smoothing_count - 1)
        seq_high <- pmin.int(seq_high, nrow(results)) # End of sequence doesn't exceed max indes.
        
        #Compute rolling average
        results_smoothed <- rowMeans(cbind(results[seq_low, "electrode"], results[seq_high, "electrode"]))
        
        results[ , 2] <- results_smoothed
        
        results
}

compare_pulse <- function(dat, fil, vmax, km, pulses, pulse_freq, release,
                          bin_size, electrode_distance, dead_space_distance,
                          diffusion_coefficient, smoothing_count,
                          convert_current, calibration_current = NULL,
                          calibration_concentration = NULL) {
        
        # One function should merge the data. merge_sim_dat
        # One function should compute the fit in r-squared, given the merged data. calc_fit
        # One function should plot the comparison given the merged data and the r-squared.
        
        mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                                  bin_size, electrode_distance, dead_space_distance,
                                  diffusion_coefficient, smoothing_count,
                                  convert_current, calibration_current,
                                  calibration_concentration)
        r2 <- calc_fit(mg)
        
        plot_rwalk_compare(mg, fil, release, vmax, km, r2,
                           calibration_current = calibration_current,
                           calibration_concentration = calibration_concentration)
                
}

read_experiment_csv <- function(fil, sr = 100, header = TRUE) {
        # sr: Sampling rate in milliseconds.
        
        # Convert sampling rate to seconds.
        sr_s <- sr * 10^-3
        
        dat <- read.csv(fil, header = header)
        
        time_sec <- seq(from = 0, by = sr_s, length.out = nrow(dat))
        
        # results <- data.frame(dat, row.names = times)
        
        # Add the time series to the front of the data frame.
        results <- as.data.frame(cbind(time_sec, dat))
        colnames(results)[2] <- "electrode"
        
        results
}

slope_intercept_df <- function(dat) {
        # dat: data frame of time stamps and electrode values.
        max_row <- nrow(dat)
        
        # Add slope column.
        dat <- cbind(dat,
                     c((dat[2:max_row, "electrode"] - dat[1:(max_row - 1), "electrode"]) /
                               (dat[2:max_row, "time_sec"] - dat[1:(max_row - 1), "time_sec"]), NA))

        colnames(dat)[3] = "slope"
        
        # Add intercept column.
        dat <- cbind(dat,
                     dat[ , "electrode"] - (dat[ , "slope"] * dat[ , "time_sec"]))
        
        colnames(dat)[4] = "intercept"
        
        dat
        }

get_slope_intercepts <- function(slp_intcpt_df, ts) {
        # df is a data frame containing slopes and intercepts of line segments.
        #  1: time_sec
        #  2. electrode. y at the electrode.
        #  3. slope
        #  4. intercept

        get1 <- function(ts_arg) {
                # Returns a data frame
                tail(slp_intcpt_df[slp_intcpt_df$time_sec < ts_arg, 3:4], 1)
        }

        # Returns a list of data frames.
        result <- lapply(ts, get1)
        
        # Merges list to a single data frame.
        result <- do.call(rbind, result)
        
        # Add time series to data frame.
        result <- cbind(time_sec = ts, result)
        
        # print(str(result))
        result
        
}

rsq <- function (x, y) cor(x, y) ^ 2

current_to_concentration <- function(current_df, calibration_current, calibration_concentration) {
        # current_df
        # Readings of current in pico amperes.
        # Data frame
        #   $time_sec
        #   $electrode
        #
        # calibration_current: in pico amperes.
        # calibration_concentration: in micro moles.
        
        # Scale and convert to concentration.
        
        electrode_first <- current_df[1, 2]
        electrode_last <- current_df[nrow(current_df), 2]
        timestamp_last <- current_df[nrow(current_df), 1]
        
        electrode_scale <- (electrode_last - electrode_first) / timestamp_last
        
        calibration_constant <- calibration_current / calibration_concentration
        
        concentration <- current_df$electrode - (electrode_first + (electrode_scale * current_df$time_sec))
        concentration <- concentration / calibration_constant
        
        current_df$electrode <- concentration

        # write.csv(current_df, file = "Data/debug_current_df.csv")
        
        current_df        
}

plot_rwalk_sim <- function(dat_w_src, release, vmax, km) {
        # dat_w_src
        # Tall data frame with column indicating source (experiment, simulation,
        #   interpolation, etc). Each source plots its own curve.

        caption <- paste("release=", release, "\n", "vmax=", vmax, "\n", "km=", km, sep = "")
        ggplot(data = dat_w_src) +
                geom_line(mapping = aes(x = time_sec, y = electrode)) +
                labs(title = "Cyclic Voltammetry Simulation",
                     x = "time [s]",
                     y = expression(paste("Concentration [", mu, "M]"))) +
                annotate("text", x = Inf, y = Inf, label = caption, vjust = 1, hjust = 1)
}

plot_rwalk_compare <- function(dat_w_src, fil, release, vmax, km, r2,
                           calibration_current = NULL, calibration_concentration = NULL) {
        # dat_w_src
        # Tall data frame with column indicating source (experiment, simulation,
        #   interpolation, etc). Each source plots its own curve.
        
        caption <- paste("release=", release, "\n", "vmax=", vmax, "\n", "km=", km, "\n",
                         "calib_curr=", calibration_current, "\n",
                         "calib_conc=", calibration_concentration, "\n",
                         "r2=", if (!is.null(r2)) {round(r2, 6)}, sep = "")
        ggplot(data = dat_w_src) +
                geom_line(mapping = aes(x = time_sec, y = electrode, colour = src)) +
                labs(title = "Cyclic Voltammetry Simulation",
                     subtitle = paste("Input Data File: ", fil),
                     x = "time [s]",
                     y = expression(paste("Concentration [", mu, "M]")),
                     colour = "source") +
                annotate("text", x = Inf, y = Inf, label = caption, vjust = 1, hjust = 1)
}

get_stim_start <- function(dat_part) {
        # dat_part: data partion taken from prior to stimulus up to the maximum stimulated reading.
        # Assume the stimulus begins at the maximum change in slope. Return the index just prior to
        # initiation of stimulus.
        
        # Row count.
        n <- nrow(dat_part)
        # Slopes of segments.
        dat_part$slope[2:n] <- dat_part$electrode[2:n] - dat_part$electrode[1:(n-1)]
        # Change in slopes.
        dat_part$delta_slope[3:n] <- dat_part$slope[3:n] - dat_part$slope[2:(n-1)]
        # Window them to shake out premature outliers. Moving average.
        dat_part$delta_slope_smooth <- mavg(dat_part$delta_slope, n = 3)

        # Largest change in slope is assumed beginning of stimulus.
        max_delta_slope_smooth <- max(dat_part$delta_slope_smooth, na.rm = TRUE)
        
        # Return the prior index.
        idx_max_delta_slope_smooth <- which(dat_part$delta_slope_smooth == max_delta_slope_smooth) - 1
        
        idx_max_delta_slope_smooth
        
}

mavg <- function(x, n=3) {
        stats::filter(x, rep(1/n, n), sides = 1)
}

rwalk_cv_pulse_run <- function(vmax, km, release, pulses,
                 pulse_freq, bin_size, electrode_distance,
                 dead_space_distance, diffusion_coefficient,
                 duration, smoothing_count) {
        
        # Don't pass smoothing_count. Not needed for rwalk matrix.
        rw <- rwalk_cv_pulse(vmax, km, release, pulses, pulse_freq, bin_size, electrode_distance,
                             dead_space_distance, diffusion_coefficient, duration)
        
        # Pass smoothing_count for the results from the electrode.
        rw_electrode <- electrode_results(rw, electrode_pos(rw), smoothing_count)
        
        rw_electrode$src <- "simulation"
        
        # Return a tall, narrow data frame with:
        #  Time series in seconds.
        #  Electrode value.
        #  Source.
        
        rw_electrode
        
}

merge_sim_dat <- function(dat, vmax, km, pulses, pulse_freq, release,
                          bin_size, electrode_distance, dead_space_distance,
                          diffusion_coefficient, smoothing_count,
                          convert_current, calibration_current = NULL,
                          calibration_concentration = NULL) {
        
        # One function should merge the data. merge_sim_dat
        # One function should compute the fit in r-squared, given the merged data.
        # One function should plot the comparison given the merged data and the r-squared.

        print("In merge_sim_dat()...")
        
        # Read data file.
        # print("Reading data...")
        # dat <- read_experiment_csv(fil, sr = sample_rate)
        
        if (convert_current) {
                dat <- current_to_concentration(dat, calibration_current, calibration_concentration)
        }
        
        # Set comparison parameters for simulation
        # Domain
        # Find the time of the first observation before the stimulus. Assume the stimulus begins at the
        # most jumpy data point. Find the maximum second derivative and take the index right before it.
        
        print("Setting up parameters of merge...")
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
        rw <- rwalk_cv_pulse_run(vmax = vmax, km = km, pulses, pulse_freq, release = release, bin_size = bin_size,
                                 electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
                                 diffusion_coefficient = diffusion_coefficient, duration = dur,
                                 smoothing_count = smoothing_count)
        
        # rwalk_cv_pulse_run returns electrode results and source.
        
        print("Formatting results...")
        # Pick off the results at the simulated electrode.
        # sim <- electrode_results(rw, electrode_pos = electrode_pos(rw), smoothing_count = 4)
        
        # Shift the time of the results.
        # sim$time_sec <- sim$time_sec + min_time
        rw$time_sec <- rw$time_sec + min_time
        
        # Make a tall data set.
        # sim_w_src <- cbind(sim, src = "simulation")
        # Don't adjust the baseline. Y starts wherever the data begins.
        # sim_w_src$electrode <- sim$electrode + y_base
        
        dat_w_src <- cbind(dat, src = "experiment")
        
        # sim_w_dat <- rbind(sim_w_src, dat_w_src)
        sim_w_dat <- rbind(rw, dat_w_src)
        
        # Return the combined tall, narrow set of simulation, experimental data, and source.
        sim_w_dat

}
        
calc_fit <- function(sim_w_dat) {
        # One function should merge the data. merge_sim_dat
        # One function should compute the fit in r-squared, given the merged data. calc_fit
        # One function should plot the comparison given the merged data and the r-squared.
        
        # write.csv(sim_w_dat[sim_w_dat$time_sec >= min_time, ], file = "Data/compare.csv")
        
        # Correlate the simulation and the experimental data.
        # Need an equal number of points on each side. Up sample the experimental data
        # based on the time series in the simulation.
        
        dat_w_src <- sim_w_dat[sim_w_dat$src == "experiment", ]
        rw_w_src <- sim_w_dat[sim_w_dat$src == "simulation", ]
        
        dat_slp_intcpt <- slope_intercept_df(dat_w_src[ , 1:2])
        
        # Build a new data frame for up sampled experimental data.
        # Time series from the model.
        #dat_up <- cbind(sim_w_src$time_sec)
        dat_up <- get_slope_intercepts(dat_slp_intcpt, rw_w_src$time_sec)
        
        # Algebra for interpolation
        m <- dat_up$slope
        x <- dat_up$time_sec
        b <- dat_up$intercept
        
        interpolate <- m * x + b
        
        dat_up <- cbind(dat_up, electrode = interpolate)
        dat_up <- cbind(dat_up, src = "interpolation", stringsAsFactors = FALSE)
        
        sim_w_datup <- rbind(rw_w_src, dat_up[ , c(1, 4, 5)])
        
        # Correlate simulation and up sampled data
        r2 <- rsq(rw_w_src[, 2], dat_up[ , 4])
        
        # Return fit
        r2
        
}

create_arg_df <- function(
                   vmax_min, vmax_max, vmax_by,
                   km_min, km_max, km_by,
                   pulses, pulse_freq,
                   release_min, release_max, release_by,
                   bin_size, electrode_distance,
                  dead_space_distance, diffusion_coefficient, smoothing_count,
                  convert_current, calibration_current, calibration_concentration){
        
        vmax <- seq(vmax_min, vmax_max, vmax_by)
        km <- seq(km_min, km_max, km_by)
        release <- seq(release_min, release_max, release_by)
        
        df <- expand.grid(release, km, vmax)
        names(df) <- c("release", "km", "vmax")
        
        df <- cbind(df, km = km, pulses = pulses, pulse_freq = pulse_freq,
                    release = release,
                    bin_size = bin_size, electrode_distance = electrode_distance,
                    dead_space_distance = dead_space_distance,
                    diffusion_coefficient = diffusion_coefficient, smoothing_count = smoothing_count,
                    convert_current = convert_current, calibration_current = calibration_current,
                    calibration_concentration = calibration_concentration, stringsAsFactors = FALSE)
        
        df <- df[c("vmax", "km", "pulses", "pulse_freq", "release", "bin_size", "electrode_distance",
                 "dead_space_distance", "diffusion_coefficient", "smoothing_count",
                 "convert_current", "calibration_current", "calibration_concentration")]

        df
}

calc_fit_multi <- function(dat, arg_df) {
        
        # Function gets the experimental data.
        #               data frame of arguments for r-squared scenarios.
        
        # lapply needs a list. Split makes a list of rows of args.
        # The anon function runs the merge function that puts together the simulation and the data.
        # do.call needs the arguments as a list.
        # For each merge, calculate 
        result <- lapply(split(arg_df, seq(nrow(arg_df))), function(x) {
                mg <- do.call(merge_sim_dat, c(list(dat), x))
                
                x$r2 <- calc_fit(mg)
                
                x
                }
               )
        
        do.call(rbind.data.frame, result)
        
}

find_stim_peaks <- function(df) {
        # Parameters
        #   df: Data frame
        #       $ time_sec
        #       $ electrode
        #
        # Returns
        #   Vector of time_sec values
        
        # Spline needs a smoothing parameter, spar, to reduce noise in stimulus.
        electrode <- smooth.spline(df$electrode, spar = .5)
        # Get the derivative.
        smoothed.dx <- predict(electrode, deriv = 1)$y
        # Where the derivative goes from negative to positive (crosses 0) is a peak.
        peaks <- which(c(smoothed.dx,NA) < 0 & c(NA, smoothed.dx) > 0) 
        
        # Return times of peaks.
        df[peaks, "time_sec"]
        
}

split_stims <- function(df) {
        # Parameters
        #   df: Data frame. From a file containing multiple stimulus events.
        #       $ time_sec
        #       $ electrode
        #
        # Returns
        #   df_list: List of data frames. Each list item is one stimulus event.
        
        peaks_time_sec <- find_stim_peaks(df)
        lead_time_sec <- peaks_time_sec[1] - df$time_sec[1]
        
        stim_start <- peaks_time_sec - lead_time_sec
        # Stimulus ends at the beginning of the next one, up to last one,
        # which is the last in the data frame.
        stim_end <- c(stim_start[-1], max(df$time_sec))
        end_points <- cbind(stim_start, stim_end)
        
        df_list <- list()
        for (row in 1:nrow(end_points)) {
                stim_start <- end_points[row, "stim_start"]
                stim_end <- end_points[row, "stim_end"]
                
                stim <- df[df$time_sec >= stim_start & df$time_sec < stim_end, ]
                df_list <- c(df_list, list(stim))
        }
        
        df_list

}

