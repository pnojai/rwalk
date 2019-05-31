input_dir <- "./input"
output_dir <- "./output"

# List experimental Excel files.
fils <- list.files(path = c("./exp/alpha-synuclein knockout_data_jai", "./exp/WT_10mgAMPH_datafiles_jai"), pattern = "xlsx$", recursive = TRUE, full.names = TRUE)

# Copy files to INPUT for conversion
lapply(fils, function(x) {file.copy(x, input_dir)})

library(openxlsx)
fils <- list.files(input_dir, pattern = "xlsx$")
for (f in fils) {
        xl <- read.xlsx(paste(input_dir, f, sep = "/"))
        fn <- paste0(strsplit(f, "\\.xlsx"), ".csv")
        write.csv(xl, file = paste0(output_dir, "/", fn), row.names = FALSE)
}