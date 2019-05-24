context("test-get-fit-boundaries")

test_that("get_fit_boundaries(). Falling phase. Sim peaks first", {
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
        convert_current = TRUE
        calibration_current = 7500.0
        calibration_concentration = 5.0
        
        mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                            bin_size, electrode_distance, dead_space_distance,
                            diffusion_coefficient,
                            convert_current, calibration_current,
                            calibration_concentration)
        
        fb <- get_fit_boundaries(mg, fit_region = "f", base_tolerance = 0.1)
        
        expect_equal(fb, c(11.44074, 34.60000), tolerance = 1.0e-6)
        
})

test_that("get_fit_boundaries(). Falling phase. Experiment peaks first", {
        fil <- "./../testdata/181015_10mg-kgAMPH_50mM_Nimo_2_1st_stim.csv"
        sample_rate <- 100
        
        dat <- read_experiment_csv(fil, sr = sample_rate)
        
        vmax <- 0
        km <- .4
        pulses <- 30
        pulse_freq <- 5
        release <- 3.05
        bin_size <- 2.0
        electrode_distance <- 50
        dead_space_distance <- 4
        diffusion_coefficient <- 2.7 * 10^-6
        convert_current = TRUE
        calibration_current = 7500.0
        calibration_concentration = 5.0
        
        mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                            bin_size, electrode_distance, dead_space_distance,
                            diffusion_coefficient,
                            convert_current, calibration_current,
                            calibration_concentration)
        
        fb <- get_fit_boundaries(mg, fit_region = "f", base_tolerance = 0.1)
        
        expect_equal(fb, c(13, 34.6), tolerance = 1.0e-6)
        
})

test_that("get_fit_boundaries(). Rising phase. Sim peaks first", {
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
        convert_current = TRUE
        calibration_current = 7500.0
        calibration_concentration = 5.0
        
        mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                            bin_size, electrode_distance, dead_space_distance,
                            diffusion_coefficient,
                            convert_current, calibration_current,
                            calibration_concentration)
        
        fb <- get_fit_boundaries(mg, fit_region = "r")
        
        expect_equal(fb, c(10.31481, 13), tolerance = 1.0e-6)
        
})

test_that("get_fit_boundaries(). Rising phase. Experiment peaks first", {
        fil <- "./../testdata/181015_10mg-kgAMPH_50mM_Nimo_2_1st_stim.csv"
        sample_rate <- 100
        
        dat <- read_experiment_csv(fil, sr = sample_rate)
        
        vmax <- 0
        km <- .4
        pulses <- 30
        pulse_freq <- 5
        release <- 3.05
        bin_size <- 2.0
        electrode_distance <- 50
        dead_space_distance <- 4
        diffusion_coefficient <- 2.7 * 10^-6
        convert_current = TRUE
        calibration_current = 7500.0
        calibration_concentration = 5.0
        
        mg <- merge_sim_dat(dat, vmax, km, pulses, pulse_freq, release,
                            bin_size, electrode_distance, dead_space_distance,
                            diffusion_coefficient,
                            convert_current, calibration_current,
                            calibration_concentration)
        
        fb <- get_fit_boundaries(mg, fit_region = "r")
        
        expect_equal(fb, c(10.31481, 16.19630), tolerance = 1.0e-6)
        
})