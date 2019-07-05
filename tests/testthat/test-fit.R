context("test-fit")

test_that("Data file splits stimuli", {
        fil <- "./../testdata/181015_10mg-kgAMPH_50mM_Nimo_2_outlier_scrub.csv"
        sample_rate <- 100
        lead_time_sec <- 10
        win_length_sec <- 119
        
        dat <- read_experiment_csv(fil, sr = sample_rate)
        dat_list <- split_stims(dat, lead_time_sec = lead_time_sec, win_length_sec = win_length_sec,
                                sr = sample_rate)
        
        expect_equal(length(dat_list), 15)
})

test_that("calc_fit() for region = all", {
        # Test a short electrode distance. We don't need to see resolution, just show that it runs.
        fil <- "./../testdata/181015_10mg-kgAMPH_50mM_Nimo_2_1st_stim.csv"
        
        sample_rate <- 100
        
        dat <- read_experiment_csv(fil, sr = sample_rate)
        
        vmax <- 0
        km <- .4
        pulses <- 30
        pulse_freq <- 50
        release <- 3.05
        bin_size <- 2.0
        electrode_distance <- 50
        dead_space_distance <- 4
        diffusion_coefficient <- 2.7 * 10^-6
        convert_current <- TRUE
        calibration_current <- 7500.0
        calibration_concentration <- 5.0
        base_tolerance <- 0.1
        
        # This data frame has the whole domain of results.
        mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                            bin_size, electrode_distance, dead_space_distance,
                            diffusion_coefficient,
                            convert_current, calibration_current,
                            calibration_concentration)
        
        expect_equal(calc_fit(mg), 0.7705312, tolerance = 1.0e-6)
})