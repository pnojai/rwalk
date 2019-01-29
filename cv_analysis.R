library(tidyverse)

fn <- "Data/AMPH_test.csv"
dat <- read_experiment_csv(fn, sr = 100)

# Set plotting parameters for simulation
# Domain
max_time <- max(dat$time_sec)
# Range
y_base <- min(dat[, 2])

rw <- rwalk_cv(vmax = 4.57, km = 0.78, release = 2.75, bin_size = 1.0,
               electrode_distance = 50, dead_space_distance = 4,
               diffusion_coefficient = 2.7*10^-6, duration = max_time)

res <- electrode_results(rw, electrode_pos = electrode_pos(rw), smoothing_count = 1)

#plot(row.names(res), res$electrode + y_base, type = "l")
#plot(row.names(dat), dat[ ,1], type = "l")

# These are programmed into the simulations now. (I hope...)
#time_series <- as.numeric(row.names(res))
#res_time <- as.data.frame(cbind(time = time_series, electrode = res$electrode))

# Plot the simulation
ggplot(data = res) +
        geom_line(mapping = aes(x = time_sec, y = electrode + y_base ))

# Plot the data
ggplot(data = dat) +
        geom_line(mapping = aes(x = time_sec, y = dat[ , 2] ))

x_sim <- res$electrode + y_base

# Superimpose them.
# Doesn't work right.
ggplot() +
        geom_line(data = res, mapping = aes(x = time_sec, y = x_sim ),
                  colour = "blue") +
        geom_line(data = dat, mapping = aes(x = time_sec, y = dat[ , 2] ),
                  colour = "red")

head(res)
head(dat)

colnames(dat)[2] <- "electrode"

res_src <- cbind(res, src = "simulation")
res_src$electrode <- res$electrode + y_base

# Let's break res_src and see where it is.
# res_src$electrode <- res_src$electrode + y_base


dat_src <- cbind(dat, src = "experiment")

head(res_src)
tail(res_src)

sim_dat <- rbind(res_src, dat_src)

sim_dat_trunc <- sim_dat[sim_dat$time_sec < 10, ]

head(sim_dat)
tail(sim_dat)

# Plot merged data. Hmmm. Simulation doesn't make it.
ggplot(data = sim_dat_trunc) +
        geom_line(mapping = aes(x = time_sec, y = electrode, colour = src))

sim_dat[sim_dat$src == "simulation", ]

ggplot(data = res_src) +
        geom_line(mapping = aes(x = time_sec, y = electrode ))

