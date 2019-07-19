context("test-preprocessing")

test_that("calculate_time_correction() works", {
        # Assume coordinate file has been read. Will pass it in.
        # Will need to read the data file. Pass the sample rate.
        # Pass stimulus period.
        
        coord_fn <- "./../testdata/1902052_a-synKO_AMPH4_CURR10400_CONC5_FILE1_peakdetection.csv"
        coord <- data.table::fread(coord_fn)
        dat_fn <- "./../testdata/1902052_a-synKO_AMPH4_CURR10400_CONC5_FILE1.csv"
        sample_rate <- 100
        stim_period <- 120 # seconds
        
        expect_equal(0, calculate_time_correction(coord, dat_fn, sample_rate, stim_period))
})

test_that("Coordinate files preprocess", {
        # File copied.
        # Has new column, included.
        # included populated with TRUE.

        coord_fn <- "1902052_a-synKO_AMPH4_CURR10400_CONC5_FILE1_peakdetection.csv"
        dat_fn <- "1902052_a-synKO_AMPH4_CURR10400_CONC5_FILE1.csv"
        input_dir <- "./../testdata"
        out_dir <- "./../../output"
        sample_rate <- 100
        stim_period <- 120 # seconds
        
        # Execute preprocess. Then test.
        x <- preprocess_coord_file(coord_fn = coord_fn, dat_fn = dat_fn,
                                   input_dir = input_dir, output_dir = output_dir,
                                   sample_rate = sample_rate, stim_period = stim_period)
        
        expect_equal(1, x)
        
})