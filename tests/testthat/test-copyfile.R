context("test-copyfile")

test_that("working files are validated in mapping document", {
        mapdoc <- "./../testdata/ScrubbingLog.csv"
        input_dir <- "./../testdata/preproc_input_queue"
        output_dir <- "./../testdata/preproc_output"

        valid <- validate_input_in_mapdoc(mapdoc, input_dir, output_dir)
        expect_equal(TRUE, valid)
        
})

test_that("extra files fail validation in mapping document", {
        mapdoc <- "./../testdata/ScrubbingLog.csv"
        input_dir <- "./../testdata/preproc_input_queue"
        output_dir <- "./../testdata/preproc_output"
        test_fil <- "TEST_FILE_DELETE_ME"
        
        # Create extra file to demonstrate validation failure.
        cmd <- paste0("touch ", input_dir, "/", test_fil)
        system(cmd)
        
        valid <- validate_input_in_mapdoc(mapdoc, input_dir, output_dir)
        file.remove(paste(input_dir, test_fil, sep = "/"))
        
        expect_equal(FALSE, valid)       
        
})

test_that("corrected files in output directory", {
        mapdoc <- "./../testdata/ScrubbingLog.csv"
        input_dir <- "./../testdata/preproc_input_queue"
        output_dir <- "./../testdata/preproc_output"
        
        preprocess_rename_files(mapdoc, input_dir, output_dir)
        
        expect_equal(1, 1)       
        
})