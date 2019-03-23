context("test-rwalk")

test_that("rwalk_cv_pulse_run does not raise error", {
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
        smoothing_count <- 4
        convert_current = TRUE
        calibration_current = 7500.0
        calibration_concentration = 5.0
        
        expect_output(compare_pulse(dat, fil = fil, vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
                                    release = release, bin_size = bin_size,
                                    electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
                                    diffusion_coefficient = diffusion_coefficient, smoothing_count = smoothing_count, convert_current = convert_current,
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
