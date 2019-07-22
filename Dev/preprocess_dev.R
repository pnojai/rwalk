library(openxlsx)
library(data.table)

# Preprocessing environment
pipeline_dir <- "./pipeline"
input_queue_dir <- paste(pipeline_dir, "02_InputQueue", sep = "/")
scrubbed_filenames_dir <- paste(pipeline_dir, "03_FileNamesScrubbed", sep = "/")
converted_files_dir <- paste(pipeline_dir, "04_FilesConverted", sep = "/")
coordinate_review_dir <- paste(pipeline_dir, "05_CoordinateReview", sep = "/")
library_dir <- paste(pipeline_dir, "06_Library", sep = "/")

# File name mapping document
mapdoc_filename <- paste(pipeline_dir, "ScrubbingLog.csv", sep = "/")
mapdoc <- as.data.frame(fread(paste(pipeline_dir, "ScrubbingLog.csv", sep = "/")))

# More global variables
tgt_extension <- "csv"
coordinate_file_tag <- "PD"
data_file_tag <- "DAT"
sample_rate <- 100
stim_period <- 120 # seconds

# Validate input queue. Are all files in the mapping document?
input_queue_is_valid <- validate_input_in_mapdoc(mapdoc = mapdoc_filename, input_dir = input_queue_dir)

if (input_queue_is_valid) {
        print("CONTINUE")
} else {
        stop("ABORT PRE-PROCESSING")
}

# Source list of file names
# Write a test that validates equality of source and target files.
# Copy source files to new file names
input_queue <- dir(input_queue_dir)
target_queue <- vector(mode = "character")
for (i in 1:length(input_queue)) {
        src_fil <- input_queue[i]
        tgt_row <- which(mapdoc$Original_FileName == src_fil)
        if (mapdoc$Accepted[tgt_row]) {
                tgt_fil <- mapdoc[tgt_row, "Original_fileName"]
        } else {
                tgt_fil <- mapdoc[tgt_row, "Scrubbed_FileName"]
        }

        target_queue[i] <- tgt_fil
        target_queue_path <- paste(scrubbed_filenames_dir, tgt_fil, sep = "/")
        
        if (file.exists(target_queue_path)) {
                print("File exists, skipping...")
                print(target_queue_path)
        }
        
        file.copy(from = paste(input_queue_dir, src_fil, sep = "/"),
                  to = paste(scrubbed_filenames_dir, tgt_fil, sep = "/"),
                  overwrite = FALSE)
}

# Advance queues
input_queue <- target_queue
target_queue <- vector(mode = "character")

# Convert Excel files to CSV.
for (i in 1:length(input_queue)) {
        fil_split <- unlist(strsplit(input_queue[i], "\\."))
        fil_name <- fil_split[1]
        fil_extension <- fil_split[2]
        
        tgt_fil_name <- paste(fil_name, tgt_extension, sep = ".")
        fil_src_path <- paste(scrubbed_filenames_dir, input_queue[i], sep = "/")
        fil_tgt_path <- paste(converted_files_dir, tgt_fil_name, sep = "/")
        target_queue[i] <- paste(fil_name, tgt_extension, sep = ".")
        
        if (file.exists(fil_tgt_path)) {
                print(paste0("Skipping : ", fil_tgt_path))
        } else {
                if (fil_extension == tgt_extension) { # Not an Excel sheet, read text data.
                        fil_dat <- fread(fil_src_path)
                } else {
                        fil_dat <- read.xlsx(fil_src_path)
                }
                print(paste0("Writing : ", fil_tgt_path))
                fwrite(x = fil_dat, file = fil_tgt_path)
        }
}

# Advance queues
input_queue <- target_queue
# Filter for coordinate files
pat <- paste0(coordinate_file_tag, "\\.", tgt_extension, "$")
input_queue <- input_queue[grep(pattern = pat, x = input_queue, ignore.case = TRUE)]
target_queue <- vector(mode = "character")

# Preprocess coordinate files.

# Review

# Read coordinate file.
## Correct the time axis. Not yet!
# Append column named include.
# Write the coordinate file.
# Copy the data file.

for (i in 1:length(input_queue)) {
        # Derive data file name.
        coord_fil <- input_queue[i]
        # coord_fil_extension <- coord_fil_split[2]
        dat_fil <- sub(pattern = paste0(coordinate_file_tag, "\\.", tgt_extension, "$"),
                            replacement = paste0(data_file_tag, "\\.", tgt_extension),
                            x = coord_fil,
                            ignore.case = TRUE)
        dat_fil_path <- paste(converted_files_dir, dat_fil, sep = "/")
        coord_fil_tgt_path <- paste(coordinate_review_dir, coord_fil, sep = "/")
        dat_fil_tgt_path <- paste(coordinate_review_dir, dat_fil, sep = "/")
        
        # Read the coordinate file
        coord <- fread(paste(converted_files_dir, coord_fil, sep = "/"))

        # This doesn't work here. Must review stimuli to ignore
        # # Igor times sometimes are displaced due to a bug.
        # # Correct them.
        # print(dat_fil_path)
        # time_correction <- calculate_time_correction(coord = coord, dat_fn = dat_fil_path,
        #                                              sample_rate = sample_rate,
        #                                              stim_period = stim_period)
        # print(time_correction)
        # # if (time_correction != 0) {
        # #         coord <- coord + time_correction
        # # }

        # Append column for toggling inclusion of a stimulus
        coord <- cbind(coord, include = TRUE)
        
        # Send files for review.
        if (file.exists(coord_fil_tgt_path)) {
                print(paste("Skipping : ", coord_fil_tgt_path))
        } else {
                print(paste("Writing : ", coord_fil_tgt_path))
                fwrite(x = coord, file = paste(coordinate_review_dir, coord_fil, sep = "/"))
        }
        # if (file.exists(dat_fil_tgt_path)) {
        #         print(paste("Skipping : ", dat_fil_tgt_path))
        # } else {
        #         print(paste("Writing : ", dat_fil_tgt_path))
        #         file.copy(from = dat_fil_path,
        #                   to = dat_fil_tgt_path,
        #                   overwrite = FALSE)
        # }
}

# Plot data files and review stimuli.
for (i in 13:length(input_queue)) {
        coord_fil <- input_queue[i]
        coord <- fread(paste(coordinate_review_dir, coord_fil, sep = "/"))
        
        # Derive data file name.
        dat_fil <- sub(pattern = paste0(coordinate_file_tag, "\\.", tgt_extension, "$"),
                       replacement = paste0(data_file_tag, "\\.", tgt_extension),
                       x = coord_fil,
                       ignore.case = TRUE)
        dat_fil_path <- paste(converted_files_dir, dat_fil, sep = "/")
        
        dat <- read_experiment_csv(dat_fil_path, sr = sample_rate)
        
        p <- ggplot(data = dat) +
                geom_line(aes(x = time_sec, y = electrode)) +
                geom_vline(xintercept = coord$T_Bkg1, color = "grey54") +
                labs(title = input_queue[i])
        print(p)
}

# Review one
input_queue
j <- 13
for (i in j:j) {
        coord_fil <- input_queue[i]
        coord <- fread(paste(coordinate_review_dir, coord_fil, sep = "/"))
        
        # Derive data file name.
        dat_fil <- sub(pattern = paste0(coordinate_file_tag, "\\.", tgt_extension, "$"),
                       replacement = paste0(data_file_tag, "\\.", tgt_extension),
                       x = coord_fil,
                       ignore.case = TRUE)
        dat_fil_path <- paste(converted_files_dir, dat_fil, sep = "/")
        
        
        dat <- read_experiment_csv(dat_fil_path, sr = sample_rate)
        dat_subset <- dat[(dat$time_sec >= 0 & dat$time_sec < 120), ]
        
        p <- ggplot(data = dat_subset) +
                geom_line(aes(x = time_sec, y = electrode)) +
                #geom_vline(xintercept = coord$T_Bkg1) +
                labs(title = input_queue[i])
        print(p)
}

# Promote to library
for (i in 1:length(input_queue)) {
        # Derive data file name.
        coord_fil <- input_queue[i]
        coord_fil_src_path <- paste(coordinate_review_dir, coord_fil, sep = "/")
        coord_fil_tgt_path <- paste(library_dir, coord_fil, sep = "/")
        dat_fil <- sub(pattern = paste0(coordinate_file_tag, "\\.", tgt_extension, "$"),
                       replacement = paste0(data_file_tag, "\\.", tgt_extension),
                       x = coord_fil,
                       ignore.case = TRUE)
        dat_fil_src_path <- paste(converted_files_dir, dat_fil, sep = "/")
        dat_fil_tgt_path <- paste(library_dir, dat_fil, sep = "/")
        
        # Copy coordinate file to library.
        if (file.exists(coord_fil_tgt_path)) {
                print(paste("Skipping : ", coord_fil_tgt_path))
        } else {
                print(paste("Copying : ", coord_fil_tgt_path))
                file.copy(from = coord_fil_src_path,
                          to = coord_fil_tgt_path,
                          overwrite = FALSE)
        }
        # Copy data file to library.
        if (file.exists(dat_fil_tgt_path)) {
                print(paste("Skipping : ", dat_fil_tgt_path))
        } else {
                print(paste("Copying : ", dat_fil_src_path))
                file.copy(from = dat_fil_src_path,
                          to = dat_fil_tgt_path,
                          overwrite = FALSE)
        }
}
