context("test-rwalk")

test_that("rwalk_cv_pulse_run does not raise error", {
        fil <- "./../testdata/181015_10mg-kgAMPH_50mM_Nimo_2_1st_stim.csv"
        
        sample_rate <- 100
        
        dat <- read_experiment_csv(fil, sr = sample_rate)
        
        vmax <- 4.8
        km <- 2.4
        pulses <- 30
        pulse_freq <- 50
        release <- 5.05
        bin_size <- 2.0
        electrode_distance <- 50
        dead_space_distance <- 4
        diffusion_coefficient <- 2.7 * 10^-6
        convert_current = TRUE
        calibration_current = 7500.0
        calibration_concentration = 5.0
        
        expect_output(compare_pulse(dat, fil = fil, vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
                                    release = release, bin_size = bin_size,
                                    electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
                                    diffusion_coefficient = diffusion_coefficient, convert_current = convert_current,
                                    calibration_current = calibration_current, calibration_concentration = calibration_concentration)
                      ,
                      "Formatting results...")
})

test_that("Michaelis-Menten function is correct", {
        expect_equal(micmen(x = 1.375, vmax = 4.57, km = .78, duration = .007407), expected = 1.353, tolerance = 0.0003)
        
})

test_that("position_releases() is correct", {
        pulses <- 30
        pulse_freq <- 50
        
        diffusion_coefficient <- 2.7 * 10^-6
        bin_size <- 2.0
        electrode_distance <- 50
        duration <- 49
        
        bin_number_displace <- as.integer(electrode_distance / bin_size)
        bins <- 2 * bin_number_displace  + 1
        it_dur <- iteration_duration(diffusion_coefficient = diffusion_coefficient, bin_size = bin_size)
        
        iterations <- as.integer(duration / it_dur)
        rw <- matrix(rep(0.0, (bins) * (iterations + 1)), iterations + 1, bins)
        
        it_dur <- iteration_duration(diffusion_coefficient = diffusion_coefficient, bin_size = bin_size)
        time_sec <- seq(from = 0, by = it_dur, length.out = nrow(rw))
        
        expected_result <- c(1,4,6,9,12,14,17,20,23,25,28,31,33,36,39,41,44,47,50,52,55,58,60,63,66,68,71,74,77,79)
        expect_equal(position_releases(pulses, pulse_freq, time_sec), expected_result)

})

test_that("get_best_fit_args is correct", {
        fil <- "./../testdata/181015_10mg-kgAMPH_50mM_Nimo_2_outlier_scrub.csv"
        sample_rate <- 100
        lead_time_sec <- 10
        win_length_sec <- 119
        
        dat <- read_experiment_csv(fil, sr = sample_rate)
        dat_list <- split_stims(dat, lead_time_sec = lead_time_sec, win_length_sec = win_length_sec, sr=sample_rate)
        
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
        
        fit_args_df <- calc_fit_multi(dat_list[[1]], arg_df) 
        
        best_args <- get_best_args(fit_args_df)
        
        best_args <- c(as.numeric(best_args["vmax"]),
                       as.numeric(best_args["km"]),
                       as.numeric(best_args["release"]))

        expect_equal(best_args, c(0.9, 6.5, 3.2))
})

test_that("compare_pulse_arg_df works", {
        fil <- "./../testdata/181015_10mg-kgAMPH_50mM_Nimo_2_outlier_scrub.csv"
        sample_rate <- 100
        lead_time_sec <- 10
        win_length_sec <- 119
        
        dat <- read_experiment_csv(fil, sr = sample_rate)
        dat_list <- split_stims(dat, lead_time_sec = lead_time_sec, win_length_sec = win_length_sec, sr=sample_rate)
        
        vmax_min <- 1.0
        vmax_max <- 1.0
        vmax_by <- 0
        km_min <- 6.5
        km_max <- 6.5
        km_by <- 0
        release_min <- 3.40
        release_max <- 3.40
        release_by <- 0
        
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
        
        fit_args_df <- calc_fit_multi(dat_list[[1]], arg_df) 
        
        best_args <- get_best_args(fit_args_df)
        
        expect_output(compare_pulse_args_df(dat_list[[1]], fil, best_args)
                      ,
                      "Formatting results...")
})

test_that("merge_sim_dat() baselines stim start", {
        sample_rate <- 100

        fil <- "./../testdata/180430_DA_saline_1.csv"
        vmax <- 4.75
        km <- 3.0
        pulses <- 30
        pulse_freq <- 50
        release <- 3.2
        bin_size <- 2
        electrode_distance <- 1000
        dead_space_distance <- 4
        diffusion_coefficient <- 2.7 * 10^-6
        convert_current = TRUE
        calibration_current = 7500.0
        calibration_concentration = 5.0
        fit_region = NULL
        base_tolerance <- 0.05
        
        # Read the train of stimuli. Break up into list. Throw away all but first.
        dat <- read_experiment_csv(fil, sr = sample_rate)
        lead_time_sec <- 10
        win_length_sec <- 119
        dat_list <- split_stims(dat, lead_time_sec = lead_time_sec, win_length_sec = win_length_sec, sr=sample_rate)
        dat <- dat_list[[1]]
        mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                            bin_size, electrode_distance, dead_space_distance,
                            diffusion_coefficient,
                            convert_current, calibration_current,
                            calibration_concentration)
        exp_start <- mg[mg$src == "experiment" & mg$time_sec == 10.0, "electrode"]
        expect_equal(exp_start, 0)
})