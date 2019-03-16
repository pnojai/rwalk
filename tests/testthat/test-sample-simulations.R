context("test-sample-simulations")

test_that("CV simulation matches sample.", {
        # Open the Excel example.
        # Read the data range you're interested in, namely the matrix.
        
        library(openxlsx)
        
        # Read simulation
        xlsx <- readWorkbook("./../../Simulations/RW_CV.xlsx", sheet = 1, startRow = 8, colNames = FALSE, cols = 5:56)

        # Make a vector of the timestamp column
        time_sec_exists <- !is.na(xlsx[,1])
        time_sec <- c(0, xlsx[time_sec_exists, 1])
        
        # Pick off odd rows, ignoring the intermediate Michaelis-Menten correction.
        result_rows_boolean <- c(rep(c(TRUE, FALSE), floor(nrow(xlsx) / 2)), TRUE)
        xlsx_results <- xlsx[result_rows_boolean, -1] # Don't use the time stamp column yet
        
        # Combine the timestamp column and the results matrix.
        xlsx_results <- cbind(time_sec, xlsx_results)
        
        # Run the simulation.
        # Random Walk.
        # Build CV model and plot it.
        
        library(tidyverse)
        
        vmax <- 4.57
        km <- 0.78
        release <- 2.75
        
        pulses <- 1
        pulse_freq <- 1
        bin_size <- 2.0
        electrode_distance <- 50
        dead_space_distance <- 4
        diffusion_coefficient <- 2.7 * 10^-6
        duration = 0.94815
        
        rw <- rwalk_cv_pulse(vmax, km, release, pulses, pulse_freq, bin_size, electrode_distance,
                             dead_space_distance, diffusion_coefficient, duration)
        
        expect_equivalent(xlsx_results, rw)
})
