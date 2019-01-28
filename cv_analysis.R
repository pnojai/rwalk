library(tidyverse)

fn <- "Data/AMPH_test.csv"
dat <- read_experiment_csv(fn, sr = 100)

# Set plotting parameters for simulation
# Domain
max_time <- as.numeric(row.names(dat)[nrow(dat)])
# Range
y_base <- min(dat[,1])

rw <- rwalk_cv(vmax = 4.57, km = 0.78, release = 2.75, bin_size = 1.0,
               electrode_distance = 50, dead_space_distance = 4,
               diffusion_coefficient = 2.7*10^-6, duration = max_time)

res <- electrode_results(rw, electrode_pos = electrode_pos(rw), smoothing_count = 1)

plot(row.names(res), res$electrode + y_base, type = "l")
plot(row.names(dat), dat[ ,1], type = "l")

time_series <- as.numeric(row.names(res))
res_time <- as.data.frame(cbind(time = time_series, electrode = res$electrode))

ggplot(data = res_time) +
        geom_line(mapping = aes(x = time, y = electrode ))

