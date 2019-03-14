context("test-plot-scripts")

test_that("Simulation script creates data.frame for plotting", {
        # Test a short electrode distance. We don't need to see resolution, just show that it runs.
        library(tidyverse)
        
        vmax <- 4.57
        km <- 0.78
        release <- 2.75
        
        pulses <- 1
        pulse_freq <- 1
        bin_size <- 2.0
        electrode_distance <- 10
        dead_space_distance <- 4
        diffusion_coefficient <- 2.7 * 10^-6
        duration = 1
        smoothing_count <- 4
        
        rw <- rwalk_cv_pulse_run(vmax, km, release, pulses, pulse_freq, bin_size, electrode_distance,
                                 dead_space_distance, diffusion_coefficient, duration)
        
        expect_is(rw, "data.frame")
})

test_that("Plotting simulation returns no error.", {
        # Test a short electrode distance. We don't need to see resolution, just show that it runs.
        library(tidyverse)
        
        vmax <- 4.57
        km <- 0.78
        release <- 2.75
        
        pulses <- 1
        pulse_freq <- 1
        bin_size <- 2.0
        electrode_distance <- 10
        dead_space_distance <- 4
        diffusion_coefficient <- 2.7 * 10^-6
        duration = 1
        smoothing_count <- 4
        
        rw <- rwalk_cv_pulse_run(vmax, km, release, pulses, pulse_freq, bin_size, electrode_distance,
                                 dead_space_distance, diffusion_coefficient, duration)
        
        expect_silent(plot_rwalk_sim(rw, release, vmax, km))
})

test_that("plot_comparison.R", {
        # Test a short electrode distance. We don't need to see resolution, just show that it runs.
        library(tidyverse)
        
        fil <- "./../../Data/181015_10mg-kgAMPH_50mM Nimo_2_1st_stim.csv"
        sample_rate <- 100
        
        dat <- read_experiment_csv(fil, sr = sample_rate)
        
        vmax <- 0
        km <- .4
        pulses <- 30
        pulse_freq <- 50
        release <- 3.05
        bin_size <- 2.0
        electrode_distance <- 10
        dead_space_distance <- 4
        diffusion_coefficient <- 2.7 * 10^-6
        smoothing_count <- 4
        convert_current = TRUE
        calibration_current = 7500.0
        calibration_concentration = 5.0
        
        compare_pulse(dat, fil = fil, vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
                      release = release, bin_size = bin_size,
                      electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
                      diffusion_coefficient = diffusion_coefficient, smoothing_count = smoothing_count, convert_current = convert_current,
                      calibration_current = calibration_current, calibration_concentration = calibration_concentration)
        
        expect_output(compare_pulse(dat, fil = fil, vmax = vmax, km = km, pulses = pulses, pulse_freq = pulse_freq,
                                    release = release, bin_size = bin_size,
                                    electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
                                    diffusion_coefficient = diffusion_coefficient, smoothing_count = smoothing_count, convert_current = convert_current,
                                    calibration_current = calibration_current, calibration_concentration = calibration_concentration)
                      ,
                      "Formatting results...") 

})