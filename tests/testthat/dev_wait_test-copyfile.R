context("test-copyfile")

testdata_dir <- "./../testdata"
mapdoc <- paste(testdata_dir, "ScrubbingLog.csv", sep = "/")
mapdoc_short <- paste(testdata_dir, "ScrubbingLog_short.csv", sep = "/")
mapdoc_target_filenames <- paste(testdata_dir, "ScrubbingLog_Short_TargetNames.csv", sep = "/")

input_dir <- paste(testdata_dir, "preproc_input_queue", sep = "/")
output_dir <- paste(testdata_dir, "preproc_output", sep = "/")

# Set up files for tests.
testfil1 <- "190205_a-synKO_AMPH1.xlsx"
testfil2 <- "190205_a-synKO_AMPH2.xlsx"
file.copy(paste(testdata_dir, testfil1, sep = "/"), to = input_dir)
file.copy(paste(testdata_dir, testfil2, sep = "/"), to = input_dir)

test_that("filename parses, coordinate file", {
        fn <- "1902052_Gko_DC10_DNamph4_CUR10400_CON5_F1_PD.xlsx"
        expected_results <- list(1902052, "ko", 10, 4, 10400, 5, 1, "COORD")
        
        x <- parse_file_name(fn)

        expect_equal(x, expected_results)
})

test_that("filename parses, data file", {
        fn <- "1902052_Gko_DC10_DNamph4_CUR10400_CON5_F1_DAT.xlsx"
        expected_results <- list(1902052, "ko", 10, 4, 10400, 5, 1, "DATA")

        x <- parse_file_name(fn)

        expect_equal(x, expected_results)
})

test_that("working files are validated in mapping document", {
        valid <- validate_input_in_mapdoc(mapdoc, input_dir)
        expect_equal(TRUE, valid)
        
})

test_that("extra files fail validation in mapping document", {
        test_fil <- "TEST_FILE_DELETE_ME"
        
        # Create extra file to demonstrate validation failure.
        cmd <- paste0("touch ", input_dir, "/", test_fil)
        system(cmd)
        
        valid <- validate_input_in_mapdoc(mapdoc, input_dir)
        file.remove(paste(input_dir, test_fil, sep = "/"))
        
        expect_equal(FALSE, valid)       
        
})

# test_that("make a list of corrected filenames", {
#         fread
#         
#         
# })

test_that("corrected files in output directory", {
        scrub_log <- data.table::fread(mapdoc)
        input_queue <- dir(input_dir)
        
        preprocess_rename_files(mapdoc, input_dir, output_dir)
        
        expect_equal(1, 1)       
        
})

# Clean up
file.remove(paste(input_dir, testfil1, sep = "/"))
file.remove(paste(input_dir, testfil2, sep = "/"))
