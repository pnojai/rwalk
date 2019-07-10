context("test-split")

test_that("split_stims() returns list of correct length", {
        fil <- "./../testdata/180430_DA_saline_1.csv"
        sample_rate <- 100
        lead_time_sec <- 10
        win_length_sec <- 119
        
        dat <- read_experiment_csv(fil, sr = sample_rate)
        
        dat_list <- split_stims(dat, lead_time_sec = lead_time_sec, win_length_sec = win_length_sec, sr = sample_rate)
        
        expect_is(dat_list, "list")
        expect_equal(length(dat_list), 5)
})