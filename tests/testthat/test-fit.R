context("test-fit")

test_that("Data file splits stimuli", {
        fil <- "./../testdata/181015_10mg-kgAMPH_50mM_Nimo_2_outlier_scrub.csv"
        
        sample_rate <- 100
        
        dat <- read_experiment_csv(fil, sr = sample_rate)
        dat_list <- split_stims(dat)
        
        expect_equal(length(dat_list), 15)
})