context("test-parsefilename")

test_that("filename parses", {
        fn <- "1902051_a-synKO_AMPH4_CURR10400_CONC5_FILE2_peakdetections.csv"
        expected_results <- list(1902051, "a-synKO", 4, 10400, 5, 2, "COORD")
        
        x <- parse_file_name(fn)
        print(str(x))
        print(x)
        
        expect_equal(x, expected_results)
})
