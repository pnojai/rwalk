library(ggplot2)

input_dir <- "./input"           # Input directory, on GitHub

# Pick a file to work on.
fils <- dir(input_dir)

sample_rate <- 100 # milliseconds
for (i in 1:length(fils)) {
        print(paste(input_dir, fils[i], sep = "/"))
        dat <- read_experiment_csv(paste(input_dir, fils[i], sep = "/"), sr = sample_rate)
        # Plot the sweep. Manually save it and view.
        p <- qplot(dat$time_sec, dat$electrode, geom = "line", main = fils[i])
        print(p)
}

