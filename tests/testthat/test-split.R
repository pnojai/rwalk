context("test-split")

test_that("split_stims() returns list of correct length", {
        fil <- "./../testdata/180430_DA_saline_1.csv"
        
        sample_rate <- 100
        
        dat <- read_experiment_csv(fil, sr = sample_rate)
        
        dat_list <- split_stims(dat)
        
        expect_is(dat_list, "list")
        expect_equal(length(dat_list), 5)
})