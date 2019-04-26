# Random Walk.
# Build CV model and plot it.
# Plot data from file.
# Report fit in terms of r-squared.

library(tidyverse)

fil <- "input/181015_10mg-kgAMPH_50mM Nimo_2_1st_stim.csv"
sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)

vmax <- 0
km <- 0.78
release <- 3.05
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

vmax_min <- 2.0
vmax_max <- 3.0
vmax_by <- 0.2
km_min <- 0.7
km_max <- 1.2
km_by <- 0.2
release_min <- 2.7
release_max <- 2.7
release_by <- 0


# Put the arguments in a list.
arg.list <- list(
        #"input/181015_10mg-kgAMPH_50mM Nimo_2_1st_stim.csv",
        # 100,
        dat,
        0,
        0.78,
        3.05,
        2.0,
        50,
        4,
        2.7 * 10^-6,
        4,
        TRUE,
        7500.0,
        5.0)

# Pass the list to the function with do.call().
mg3 <- do.call(merge_sim_dat, arg.list)

# Works. Maybe I can make a list of arg lists and lapply to get back a list of results.
# Does it work if I put the args in a data frame?
arg_df <- create_arg_df(vmax_min, vmax_max, vmax_by, km_min, 
                        km_max, km_by, release_min, release_max, release_by, bin_size, 
                        electrode_distance, dead_space_distance, diffusion_coefficient, 
                        convert_current, calibration_current, calibration_concentration)

head(arg_df)

# call_args <- list(dat)
# c(call_args, arg_df[1, ])

# mg3 <- do.call(merge_sim_dat, c(call_args, arg_df[1, ])) #as.list(call_args))
mg3 <- do.call(merge_sim_dat, c(list(dat), arg_df[1, ])) #as.list(call_args))

lapply(split(arg_df, seq(nrow(arg_df))), function(x) {
        mg <- do.call(merge_sim_dat, c(list(dat), x))
        calc_fit(mg)
        }
       )


mg1 <- merge_sim_dat(fil, sample_rate, vmax, km, release, bin_size, electrode_distance,
                     dead_space_distance, diffusion_coefficient,
                     convert_current, calibration_current, calibration_concentration)

compare_pulse(fil = fil, sample_rate = sample_rate, vmax = vmax, km = km,
        release = release, bin_size = bin_size,
        electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
        diffusion_coefficient = diffusion_coefficient,
        calibration_current = calibration_current, calibration_concentration = calibration_concentration)

#####

library(tidyverse)

fil <- "./input/181015_10mg-kgAMPH_50mM Nimo_2_outlier_scrub.csv"
sample_rate <- 100

dat <- read_experiment_csv(fil, sr = sample_rate)
dat_list <- split_stims(dat)

plot(dat$time_sec, dat$electrode, type = "l")

vmax_min <- 0.9
vmax_max <- 1.0
vmax_by <- .1
km_min <- 6.5
km_max <- 6.5
km_by <- 0
release_min <- 3.1
release_max <- 3.2
release_by <- .1

pulses <- 30
pulse_freq <- 50
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
convert_current = TRUE
calibration_current = 7500.0
calibration_concentration = 5.0

arg_df <- create_arg_df(vmax_min, vmax_max, vmax_by, km_min, 
                        km_max, km_by, 
                        pulses, pulse_freq,
                        release_min, release_max, release_by, bin_size, 
                        electrode_distance, dead_space_distance, diffusion_coefficient, 
                        convert_current, calibration_current, calibration_concentration)

arg_df

fit_args_df <- calc_fit_multi(dat_list[[1]], arg_df) 

fit_args_df

ncol(fit_args_df)

best_fit_df <- arrange(fit_args_df, desc(r2))[1 , -14]
best_fit_df

best_args_df <- get_best_args(fit_args_df)

do.call(compare_pulse, c(list(dat_list[[1]]), fil, best_fit_df))

compare_pulse(dat_list[[1]], fil = fil, vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
              release = release, bin_size = bin_size,
              electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
              diffusion_coefficient = diffusion_coefficient, convert_current = convert_current,
              calibration_current = calibration_current, calibration_concentration = calibration_concentration)

compare_pulse_args_df(dat_list[[1]], paste(fil, "#1"), best_args_df)
