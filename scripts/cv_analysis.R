library(tidyverse)

fn <- "input/AMPH_test.csv"
dat <- read_experiment_csv(fn, sr = 100)

# Set plotting parameters for simulation
# Domain
max_time <- max(dat$time_sec)

# Range
y_base <- min(dat[, 2])

rw <- rwalk_cv(vmax = 4.57, km = .78, release = 6000, bin_size = 1.0,
               electrode_distance = 50, dead_space_distance = 4,
               diffusion_coefficient = 2.7*10^-6, duration = max_time)

res <- electrode_results(rw, electrode_pos = electrode_pos(rw), smoothing_count = 4)

# Plot the simulation
ggplot(data = res) +
        geom_line(mapping = aes(x = time_sec, y = electrode + y_base ))

# Plot the data
ggplot(data = dat) +
        geom_line(mapping = aes(x = time_sec, y = dat[ , 2] ))

# Make a tall data set.
res_w_src <- cbind(res, src = "simulation")
res_w_src$electrode <- res$electrode + y_base

dat_w_src <- cbind(dat, src = "experiment")

sim_w_dat <- rbind(res_w_src, dat_w_src)

# Superimpose them.
# Two data sets
ggplot() +
        geom_line(data = res, mapping = aes(x = time_sec, y = electrode + y_base),
                  colour = "blue") +
        geom_line(data = dat, mapping = aes(x = time_sec, y = dat[ , 2] ),
                  colour = "red")

# Merged data.
ggplot(data = sim_w_dat) +
        geom_line(mapping = aes(x = time_sec, y = electrode, colour = src))

# Do this again, but trim the front off the experimental data
fn <- "input/AMPH_test.csv"
dat <- read_experiment_csv(fn, sr = 100)

# Set plotting parameters for simulation
# Domain
# Find the time of the minimum observation before the peak.
idx_max_obs <- which(dat$electrode == max(dat$electrode)) # Index of peak
# Get the minimum observation in the 1st partition. Find the index.
idx_min_obs <- which(dat$electrode == min(dat[1:idx_max_obs, 2]))
idx_min_obs <- idx_min_obs[idx_min_obs < idx_max_obs] # Min obs earlier than peak.

min_time <- dat[idx_min_obs, "time_sec"]
max_time <- max(dat$time_sec)

# Range
y_base <- dat[idx_min_obs, 2]

# RESUME HERE. SHIFT THE SIMULATION TO THE WINDOW OF EXPERIMENTAL ACTIVITY.

# Trim experimental data. No, don't trim it. Add time to the simulation.
# dat <- dat[-(1:(idx_min_obs - 1)), ]

dur <- max_time - min_time

rw <- rwalk_cv(vmax = 4.57, km = .78, release = 6000, bin_size = 1.0,
               electrode_distance = 50, dead_space_distance = 4,
               diffusion_coefficient = 2.7*10^-6, duration = dur)

res <- electrode_results(rw, electrode_pos = electrode_pos(rw), smoothing_count = 4)

# Shift the time of the results.
res$time_sec <- res$time_sec + min_time

# Plot the simulation
ggplot(data = res) +
        geom_line(mapping = aes(x = time_sec, y = electrode + y_base ))

# Plot the data
ggplot(data = dat) +
        geom_line(mapping = aes(x = time_sec, y = dat[ , 2] ))

# Make a tall data set.
res_w_src <- cbind(res, src = "simulation")
res_w_src$electrode <- res$electrode + y_base

dat_w_src <- cbind(dat, src = "experiment")

sim_w_dat <- rbind(res_w_src, dat_w_src)

# Superimpose them.
# Two data sets
ggplot() +
        geom_line(data = res, mapping = aes(x = time_sec, y = electrode + y_base),
                  colour = "blue") +
        geom_line(data = dat, mapping = aes(x = time_sec, y = dat[ , 2] ),
                  colour = "red")

# Merged data.
# Greek letters. Here's how to include them in labels.
# https://stats.idre.ucla.edu/r/codefragments/greek_letters/
ggplot(data = sim_w_dat) +
        geom_line(mapping = aes(x = time_sec, y = electrode, colour = src)) +
        labs(title = "cyclic voltammetry",
             x = "time [s]",
             y = expression(paste("DA concentration [", mu, "M]")),
             colour = "source")

# Work out interpolation.
head(dat)
dat <- slope_intercept_df(dat)
tail(dat)
