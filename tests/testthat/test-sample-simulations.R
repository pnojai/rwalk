context("test-sample-simulations")

# Test passed 5/3. Then filtered the zero rows. The electrode test works and is all
# that is necessary now.
# test_that("CV simulation matrix matches sample.", {
#         # Open the Excel example.
#         # Read the data range you're interested in, namely the matrix.
#         # The optimized approach eliminates the right half of the matrix. Just consider the left.
# 
#         library(openxlsx)
#         library(readr)
# 
#         # Read simulation
#         xlsx <- readWorkbook("./../testdata/RW_CV_corrected.xlsx", sheet = 1, startRow = 8,
#                              skipEmptyRows = FALSE, colNames = FALSE, cols = c(5:31))
#         
#         # Make a vector of the timestamp column
#         time_sec_exists <- !is.na(xlsx[,1])
#         time_sec <- c(0, xlsx[time_sec_exists, 1])
# 
#         # Pick off odd rows, ignoring the intermediate Michaelis-Menten correction.
#         result_rows_boolean <- c(rep(c(TRUE, FALSE), floor(nrow(xlsx) / 2)), TRUE)
#         xlsx_results <- xlsx[result_rows_boolean, -1] # Don't use the time stamp column yet
# 
#         # Combine the timestamp column and the results matrix.
#         xlsx_results <- cbind(time_sec, xlsx_results)
#         xlsx_results[is.na(xlsx_results)] <- 0
# 
#         write_csv(xlsx_results, "./../testdata/RW_CV_simulation_as_read.csv")        
#         
#         # Run the simulation.
#         # Random Walk.
#         # Build CV model and plot it.
# 
#         library(tidyverse)
# 
#         vmax <- 4.57
#         km <- 0.78
#         release <- 2.75
# 
#         pulses <- 1
#         pulse_freq <- 1
#         bin_size <- 2.0
#         electrode_distance <- 50
#         dead_space_distance <- 4
#         diffusion_coefficient <- 2.7 * 10^-6
#         duration = 0.94815
# 
#         rw <- rwalk_cv_pulse(vmax, km, release, pulses, pulse_freq, bin_size, electrode_distance,
#                              dead_space_distance, diffusion_coefficient, duration)
# 
#         write_csv(rw, "./../testdata/RW_CV_rwalk_computed.csv") 
#         
#         rw_left_side = rw[, 1:27] # Disregard bins to the right of the electrode
#         
#         expect_equivalent(xlsx_results, rw_left_side)
# })

test_that("CV simulation electrode matches sample", {
        # Open the Excel example.
        # Read the data range you're interested in, namely the matrix.
        
        library(openxlsx)
        library(readr)
        
        # Read simulation
        xlsx <- readWorkbook("./../testdata/RW_CV_corrected.xlsx", sheet = 1, startRow = 8,
                             skipEmptyRows = FALSE, colNames = FALSE, cols = c(5, 31))

        # Make a vector of the timestamp column
        time_sec_exists <- !is.na(xlsx[,1])
        time_sec <- c(0, xlsx[time_sec_exists, 1])
        
        # Pick off odd rows, ignoring the intermediate Michaelis-Menten correction.
        result_rows_boolean <- c(rep(c(TRUE, FALSE), floor(nrow(xlsx) / 2)), TRUE)
        xlsx_results <- xlsx[result_rows_boolean, -1] # Don't use the time stamp column yet
        
        # Combine the timestamp column and the results matrix.
        xlsx_results <- as.data.frame(cbind(time_sec, xlsx_results))

        # write_csv(xlsx_results, "./../testdata/RW_CV_simulation_electrode_as_read.csv")    
        
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
        
        rw <- rw[ , c(1, 27)]
        xlsx_results[is.na(xlsx_results[, 2]), 2] <- 0
        
        # write_csv(rw, "./../testdata/RW_CV_rwalk_electrode_computed.csv") 
        
        # Spreadsheet by design only uses every other time iteration at the electrod.
        included <- seq(1, nrow(xlsx_results), 2)
        
        
        expect_equivalent(xlsx_results[included, ], rw)
})

test_that("CV simulation releases match corrected sample", {
        # Open the Excel example.
        # Read the data range you're interested in, namely the matrix.
        
        library(openxlsx)
        library(readr)
        
        # Read simulation
        # readWorkbook() argument cols subsets incorrectly, maybe because skipEmptyCols is set to FALSE.
        # Read row with releases, all columns. Then subset rows 5:31.
        xlsx <- readWorkbook("./../testdata/RW_CV_corrected.xlsx", sheet = 1, rows = 8, colNames = FALSE, skipEmptyCols = FALSE)
        
        xlsx <- xlsx[ , 6:31]
        xlsx[is.na(xlsx)] <- 0

        # Run the simulation.
        # Random Walk.
 
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
        
        # Release row in provided file.
        # print(xlsx)
        # Release row in rwalk.
        # print(rw[1, 2:27])
        
        expect_equivalent(xlsx, rw[1, 2:27])
        
})
