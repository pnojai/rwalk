#' Michaelis-Menten
#' 
#' Correct for uptake according to the Michaelis-Menten equation.
#'
#' @param x Concentration in micromoles
#' @param vmax Micromoles
#' @param km Micromoles
#' @param duration Seconds
#'
#' @return numeric
#' @export
#'
#' @examples micmen(x = 1.375, vmax = 4.57, km = .78, duration = .007407)
micmen <- function(x, vmax, km, duration) {
        x - ((( vmax * x ) / ( km + x )) * duration )
        
}

#' Iteration duration
#' 
#' Return the length of time between iterations of a random walk matrix.
#' Given by: t = x^2 / 2D
#'
#' @param diffusion_coefficient Square centimeters per second.
#' @param bin_size Distance between release bins in microns.
#'
#' @return numeric
#' @export
#'
#' @examples
iteration_duration <- function(diffusion_coefficient, bin_size) {
        # diffusion_coefficient: square centimeters / second.
        # bin_size: micrometres.
        # t = x^2/2D
        
        ((bin_size / 10000.0)^2) / (2 * diffusion_coefficient)
}

#' Build a random walk matrix for cyclic voltammetry
#' 
#' The random walk models concentration at time intervals and bin distances
#' from an electrode. For performance optimization, only bins to the left of
#' the electrode are included in the matrix, as the right bin values are
#' symmetric with respect to the left.
#' 
#' Given a pulse train, the function prorates the given release evenly to the
#' nearest appropriate time iteration.
#' 
#' Resolution of the matrix can be customized with the parameters bin_size and
#' electrode_distance.
#' 
#' The function supports an assumption of dead space at the insertion point
#' of the electrode.
#'
#' @param vmax For Michaelis-Menten correction of uptake. Microns per second.
#' @param km For Michaelis-Menten. Microns.
#' @param release Concentration in micromoles.
#' @param pulses Count of pulses in the pulse train.
#' @param pulse_freq Pulse frequency in Hz.
#' @param bin_size Spacing of the modelled release bins in microns.
#' @param electrode_distance Distance from the electrode to the last bin in microns.
#' @param dead_space_distance Distance in microns of one side of the dead space measured
#' from the electrode.
#' @param diffusion_coefficient Diffusion constant in square centimeters per second.
#' @param duration Span of time for modelling release diffusion measured in seconds.
#'
#' @return rw_df. Data frame of the modelled random walk. The first column is
#' rw_df$time_sec, the time in seconds of the iteration. The results reside in the
#' values modelled at the location of the electrode in column rw_df$electrode.
#' @export
#'
#' @examples
rwalk_cv_pulse <- function(vmax, km, release, pulses,
                           pulse_freq, bin_size, electrode_distance,
                           dead_space_distance, diffusion_coefficient,
                           duration) {

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
        
        # Closest timestamps in matrix for releases.
        release_time_sec_idx <- position_releases(pulses, pulse_freq, time_sec)
        
        # Position the electrode and the dead space
        electrode_pos <- electrode_pos(rw, time_column = FALSE) # No time series on the front yet.
        
        # Identify dead spaces in a logical vector
        dead_space_displace <- as.integer(dead_space_distance / bin_size)
        dead_space_range <- (bin_number_displace - dead_space_displace + 1):(bin_number_displace + dead_space_displace + 1)
        dead_space_bin <- rep(FALSE, bins)
        dead_space_bin[dead_space_range] <- TRUE
        
        # Release at time 0 (row 1).
        # Don't release to dead space.
        # Don't release to odd-numbered bins since Eugene's fix.
        release_bin <- c(rep(c(FALSE, TRUE), floor(bins / 2)), FALSE)
        rw[1, release_bin & !dead_space_bin] <- release_timed
        
        # Iterate in time
        for (i in 2:(iterations + 1)) {
                # Fill extreme bins.
                # Outside bins take from inside neighbor only
                curr_bin <- 1
                inside_neighbor <- curr_bin + 1
                if ((i - 1) %% 2 == 1) {
                        val <- mean(c(rw[(i - 1), inside_neighbor], 0))
                        val <- micmen(val, vmax, km, it_dur)
                } else {
                        val <- 0
                }
                rw[i, 1] <- val
                if (i %in% release_time_sec_idx & !dead_space_bin[1]) {
                        #print(paste("Releasing in time index:", i))
                        rw[i, 1] <- rw[i, 1] + release_timed
                }
                
                # 2nd bins in take .711 from outside neighbor, .5 from inside
                # curr_bin <- 2
                # inside_neighbor <- curr_bin + 1
                # outside_neighbor <- curr_bin - 1
                # # i %% 2 toggles inclusion
                # if ((i %% 2) == 1) {
                #         val <- .711 * rw[(i - 1), outside_neighbor] + .5 * rw[(i - 1), inside_neighbor]
                #         val <- micmen(val, vmax, km, it_dur)
                # } else {
                #         val <- 0
                # }
                # rw[i, curr_bin] <- val
                # if (i %in% release_time_sec_idx & !dead_space_bin[curr_bin]) {
                #         #print(paste("Releasing in time index:", i))
                #         rw[i, curr_bin] <- rw[i, curr_bin] + release_timed
                # }
                
                # Diffuse the molecules until you get to the electrode.
                # Vectorized this.
                rw_outside_neighbor <- rw[(i - 1), (2 - 1):(electrode_pos - 1 - 1)]
                rw_inside_neighbor <- rw[(i - 1), (2 + 1):(electrode_pos - 1 + 1)]
                val_v <- rowMeans(cbind(rw_outside_neighbor, rw_inside_neighbor))
                val_v<- micmen(val_v, vmax, km, it_dur)
                
                rw[i, 2:(electrode_pos - 1)] <- val_v
                
                if (i %in% release_time_sec_idx) {
                        #print(paste("Releasing in time index:", i))
                        val_v[!dead_space_bin[2:(electrode_pos - 1)]] <- val_v[!dead_space_bin[2:(electrode_pos - 1)]] + release_timed
                        rw[i, 2:(electrode_pos - 1)] <- val_v
                }
                
                # Since the neighbors are symmetric, averaging them is the same as taking
                # the whole of one neighbor.
                val <- rw[i - 1, electrode_pos - 1]
                
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
        
        # Include every other row of the matrix. (Excluding the empty rows.)
        included <- seq(1, nrow(rw_df), 2)
        
        rw_df[included, ]
        # rw_df
}

#' Electrode position
#' 
#' The index of the column in the random walk matrix representing the location
#' of the electrode. The model locates the electrode in the middle of a
#' one-dimensional string of release points. However, for performance
#' optimization, the release bins to the right of the electrode are not
#' populated. They are not needed for the calculation of the electrode results,
#' since the bin values to the right and left of the electrode are symmetric.
#'
#' @param rw. A calculated random walk matrix in the form of a data frame.
#' The electrode bin is the middle column of the data frame.
#' @param time_column. Logical. Indicates the presence of a time series column
#' at the front of the data frame.
#'
#' @return Numeric. The column index indicating the location in the data frame
#' of the electrode results.
#' @export
#'
#' @examples
electrode_pos <- function(rw, time_column = TRUE) {
        # Electrode is the middle bin.
        if (time_column == TRUE) {
                pos <- (ncol(rw) / 2) + 1
        } else {
                pos <- ((ncol(rw) - 1)/ 2) + 1
        }
        
        pos
}

#' Electrode results
#' 
#' Returns the time series and electrode results of a given random walk
#' matrix. The rest of the matrix serves only to compute the electrode
#' results and are unnecessary for subsequent plotting.
#'
#' @param rwalk_df. A calculated random walk matrix in the form of a
#' data frame.
#' @param electrode_pos. The column index within the random walk matrix,
#' locating the electrode results. 
#'
#' @return
#' @export
#'
#' @examples
electrode_results <- function(rwalk_df, electrode_pos) {
        results <- as.data.frame(
                cbind(time_sec = rwalk_df[-1, "time_sec"],
                      electrode = rwalk_df[-1, "electrode"]
                      )
                )
        
        results
        
}

#' Title
#'
#' @param dat 
#' @param fil 
#' @param vmax 
#' @param km 
#' @param pulses 
#' @param pulse_freq 
#' @param release 
#' @param bin_size 
#' @param electrode_distance 
#' @param dead_space_distance 
#' @param diffusion_coefficient 
#' @param convert_current 
#' @param calibration_current 
#' @param calibration_concentration 
#'
#' @return
#' @export
#'
#' @examples
compare_pulse <- function(dat, fil, vmax, km, pulses, pulse_freq, release,
                          bin_size, electrode_distance, dead_space_distance,
                          diffusion_coefficient,
                          convert_current, calibration_current = NULL,
                          calibration_concentration = NULL) {
        
        # One function should merge the data. merge_sim_dat
        # One function should compute the fit in r-squared, given the merged data. calc_fit
        # One function should plot the comparison given the merged data and the r-squared.
        
        mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                                  bin_size, electrode_distance, dead_space_distance,
                                  diffusion_coefficient,
                                  convert_current, calibration_current,
                                  calibration_concentration)
        r2 <- calc_fit(mg)
        
        
        plot_rwalk_compare(mg, fil, release, vmax, km, r2,
                           calibration_current = calibration_current,
                           calibration_concentration = calibration_concentration)
                
}

#' Title
#'
#' @param fil 
#' @param sr 
#' @param header 
#'
#' @return
#' @export
#'
#' @examples
read_experiment_csv <- function(fil, sr = 100, header = TRUE) {
        # sr: Sampling rate in milliseconds.
        
        # Convert sampling rate to seconds.
        sr_s <- sr * 10^-3
        
        dat <- utils::read.csv(fil, header = header)
        
        time_sec <- seq(from = 0, by = sr_s, length.out = nrow(dat))
        
        # results <- data.frame(dat, row.names = times)
        
        # Add the time series to the front of the data frame.
        results <- as.data.frame(cbind(time_sec, dat))
        colnames(results)[2] <- "electrode"
        
        results
}

#' Title
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param slp_intcpt_df 
#' @param ts 
#'
#' @return
#' @export
#'
#' @examples
get_slope_intercepts <- function(slp_intcpt_df, ts) {
        # df is a data frame containing slopes and intercepts of line segments.
        #  1: time_sec
        #  2. electrode. y at the electrode.
        #  3. slope
        #  4. intercept

        get1 <- function(ts_arg) {
                # Returns a data frame
                utils::tail(slp_intcpt_df[slp_intcpt_df$time_sec < ts_arg, 3:4], 1)
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

rsq <- function (x, y) stats::cor(x, y) ^ 2

#' Title
#'
#' @param current_df 
#' @param calibration_current 
#' @param calibration_concentration 
#'
#' @return
#' @export
#'
#' @examples
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

        # utils::write.csv(current_df, file = "input/debug_current_df.csv")
        
        current_df        
}

#' Title
#'
#' @param dat_w_src 
#' @param release 
#' @param vmax 
#' @param km 
#'
#' @return
#' @export
#'
#' @examples
plot_rwalk_sim <- function(dat_w_src, release, vmax, km) {
        # dat_w_src
        # Tall data frame with column indicating source (experiment, simulation,
        #   interpolation, etc). Each source plots its own curve.

        caption <- paste("release=", release, "\n", "vmax=", vmax, "\n", "km=", km, sep = "")
        ggplot2::ggplot(data = dat_w_src) +
                ggplot2::geom_line(mapping = ggplot2::aes(x = time_sec, y = electrode)) +
                ggplot2::labs(title = "Cyclic Voltammetry Simulation",
                     x = "time [s]",
                     y = expression(paste("Concentration [", mu, "M]"))) +
                ggplot2::annotate("text", x = Inf, y = Inf, label = caption, vjust = 1, hjust = 1)
}

#' Title
#'
#' @param dat_w_src 
#' @param fil 
#' @param release 
#' @param vmax 
#' @param km 
#' @param r2 
#' @param calibration_current 
#' @param calibration_concentration 
#'
#' @return
#' @export
#'
#' @examples
plot_rwalk_compare <- function(dat_w_src, fil, release, vmax, km, r2,
                           calibration_current = NULL, calibration_concentration = NULL) {
        # dat_w_src
        # Tall data frame with column indicating source (experiment, simulation,
        #   interpolation, etc). Each source plots its own curve.
        
        caption <- paste("release=", release, "\n", "vmax=", vmax, "\n", "km=", km, "\n",
                         "calib_curr=", calibration_current, "\n",
                         "calib_conc=", calibration_concentration, "\n",
                         "r2=", if (!is.null(r2)) {round(r2, 6)}, sep = "")
        ggplot2::ggplot(data = dat_w_src) +
                ggplot2::geom_line(mapping = ggplot2::aes(x = time_sec, y = electrode, colour = src)) +
                ggplot2::labs(title = "Cyclic Voltammetry Simulation",
                     subtitle = paste("Input Data File: ", fil),
                     x = "time [s]",
                     y = expression(paste("Concentration [", mu, "M]")),
                     colour = "source") +
                ggplot2::annotate("text", x = Inf, y = Inf, label = caption, vjust = 1, hjust = 1)
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
        
        # n = 3 sometimes doesn't find the start. Trying n = 5.
        dat_part$delta_slope_smooth <- mavg(dat_part$delta_slope, n = 5)

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
                 duration) {
        
        rw <- rwalk_cv_pulse(vmax, km, release, pulses, pulse_freq, bin_size, electrode_distance,
                             dead_space_distance, diffusion_coefficient, duration)
        
        rw_electrode <- electrode_results(rw, electrode_pos(rw))
        
        rw_electrode$src <- "simulation"
        
        # Return a tall, narrow data frame with:
        #  Time series in seconds.
        #  Electrode value.
        #  Source.
        
        rw_electrode
        
}

merge_sim_dat <- function(dat, vmax, km, pulses, pulse_freq, release,
                          bin_size, electrode_distance, dead_space_distance,
                          diffusion_coefficient,
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
                
                utils::write.csv(dat[1:idx_max_obs, ], "input/debug_datToMax")
        }
        
        # Calculate random walk.
        print("Building random walk...")
        rw <- rwalk_cv_pulse_run(vmax = vmax, km = km, pulses, pulse_freq, release = release, bin_size = bin_size,
                                 electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
                                 diffusion_coefficient = diffusion_coefficient, duration = dur)
        
        # rwalk_cv_pulse_run returns electrode results and source.
        
        print("Formatting results...")
        # Pick off the results at the simulated electrode.
        # sim <- electrode_results(rw, electrode_pos = electrode_pos(rw))
        
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
        
        # utils::write.csv(sim_w_dat[sim_w_dat$time_sec >= min_time, ], file = "input/compare.csv")
        
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
                  dead_space_distance, diffusion_coefficient,
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
                    diffusion_coefficient = diffusion_coefficient,
                    convert_current = convert_current, calibration_current = calibration_current,
                    calibration_concentration = calibration_concentration, stringsAsFactors = FALSE)
        
        df <- df[c("vmax", "km", "pulses", "pulse_freq", "release", "bin_size", "electrode_distance",
                 "dead_space_distance", "diffusion_coefficient",
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
        electrode <- stats::smooth.spline(df$electrode, spar = .5)
        # Get the derivative.
        smoothed.dx <- stats::predict(electrode, deriv = 1)$y
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

position_releases <- function(pulses, pulse_freq, time_sec) {
        # Time between pulses.
        pulse_dur <- 1.0 / pulse_freq

        # Times for releases.
        releases <- 0:(pulses - 1) * pulse_dur
        
        # Closest timestamps in matrix for releases.
        result <- sapply(releases, function(x) {which.min(abs(time_sec - x))})
        
        result
}

#' Get arguments for best fit
#' 
#' Given a data frame of arguments for building a random walk model, return
#' the ones that fit best to the experimental data determined by their r-squared
#' value. 
#'
#' @param arg_df Data frame. Calculated by calc_fit_multi(). One row contains
#' the arguments for building a random walk model. Column arg_df$r2 is the
#' r-squared value correlating the model built by those arguments and the experimental
#' data.
#'
#' @return One-row data frame.
#' @export
#'
#' @examples
get_best_args <- function(arg_df) {
        result <- plyr::arrange(arg_df, plyr::desc(r2))[1 , -13]
        result
}

#' Compare random walk to experimental data (arguments in data frame)
#' 
#' A wrapper for compare_pulse, supporting an alternate protocol with
#' the random walk arguments supplied in a one-row data frame.
#'
#' @param dat Data frame. Experimental data for one stimulus supplied by
#' read_experiment_csv().
#'  
#' @param fil Character. String representing the file which supplied the 
#' dat data frame. For annotation of the plot.
#' 
#' @param args_df One-row data frame. Arguments for calculating a random
#' walk. 
#'
#' @return
#' @export
#'
#' @examples
compare_pulse_args_df <- function(dat, fil, args_df) {
        do.call(compare_pulse, c(list(dat), fil, args_df))
}

set_fit_boundaries <- function(sim_w_dat, range, base_tolerance) {
        if (range %in% c("r", "rise")) {
                print("Rise phase")
                result <- c(0,0)
        } else if (range %in% c("f", "fall")) {
                # Times for the peaks. Take min of each in case the peak is reached more than once.
                peak_time_sim <- min(sim_w_dat$time_sec[sim_w_dat$electrode == max(sim_w_dat$electrode[sim_w_dat$src == "simulation"])])
                peak_time_exp <- min(sim_w_dat$time_sec[sim_w_dat$electrode == max(sim_w_dat$electrode[sim_w_dat$src == "experiment"])])
                peak_time_min <- min(peak_time_sim, peak_time_exp)
                
                # Time for simulation arrival at baseline plus base_tolerance.
                base_time_sim <- max(sim_w_dat$time_sec[sim_w_dat$electrode >= base_tolerance & sim_w_dat$src == "simulation"])
                
                result <- c(min(c(peak_time_sim, peak_time_exp)), base_time_sim) # c(11.84074, 25.15926) 
                #result <- c(11.84074, 25.15926) 
        } else {
                print("All")
        }
        result
}