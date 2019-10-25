library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)

pipeline_dir <- "/media/sf_OneDrive_-_cumc.columbia.edu/rwalk/pipeline"
input_dir <- paste(pipeline_dir, "06_Library", sep = "/")
output_dir <- file.path(pipeline_dir, "output")
sample_rate <- 100

dat_fils <- dir(path = input_dir, pattern = "DAT")
coord_fils <- dir(path = input_dir, pattern = "PD")

is_not_match <- sum(str_sub(dat_fils, start = 1, end = 10) != str_sub(coord_fils, start = 1, end = 10))

if (is_not_match) {stop("Files out of order.")}

for (i in 1:length(dat_fils)) {
        dat <- read_experiment_csv(file.path(input_dir, dat_fils[i]), sr = sample_rate)
        coord <- fread(file.path(input_dir, coord_fils[i]))
        
        ggplot() +
                geom_line(data = dat, mapping = aes(x = time_sec, y = electrode), size = .1) +
                geom_vline(data = coord, xintercept = coord$T_Bkg1, color = "red", size = .1) +
                labs(title = str_sub(dat_fils[i], 1, 10))
        
        ggsave(filename = str_c(output_dir, "/", str_sub(dat_fils[i], 1, 10), ".pdf"))
}

for (j in 1:nrow(coord)) {
        print(j)
        stim_start <- coord$T_Bkg1[j] 
        dat_one_stim <- subset(dat, subset = between(time_sec, stim_start, stim_start + 120))
        
        p <- ggplot() +
        geom_line(data = dat_one_stim, mapping = aes(x = time_sec, y = electrode)) +
        labs(title = str_c(str_sub(dat_fils[i], 1, 10), " Stim: ", j))

        print(p)
        
}


