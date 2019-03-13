library(tidyverse)

fil <- "Data/AMPH_test.csv"
sample_rate <- 100
vmax <- 4.57
km <- 0.78
release <- 6100 # 2.75
bin_size <- 2.0
electrode_distance <- 50
dead_space_distance <- 4
diffusion_coefficient <- 2.7 * 10^-6
smoothing_count <- 4

# Read data file.
dat <- read_experiment_csv(fil, sr = sample_rate)

# Set plotting parameters for simulation
# Domain
# Find the time of the minimum observation before the peak.
max_obs <- max(dat$electrode)
idx_max_obs <- which(dat$electrode == max_obs) # Index of peak
# Get the minimum observation in the 1st partition. Find the index.
idx_min_obs <- which(dat$electrode == min(dat[1:idx_max_obs, 2]))
idx_min_obs <- idx_min_obs[idx_min_obs < idx_max_obs] # Min obs earlier than peak.

min_time <- dat[idx_min_obs, "time_sec"]
max_time <- max(dat$time_sec)

# Range
y_base <- dat[idx_min_obs, 2]

# Duration of simulation.
dur <- max_time - min_time

# Calculate random walk.
rw <- rwalk_cv(vmax = vmax, km = km, release = release, bin_size = bin_size,
               electrode_distance = electrode_distance, dead_space_distance = dead_space_distance,
               diffusion_coefficient = diffusion_coefficient, duration = dur)

str(rw)
# Pick off the results at the simulated electrode.
res <- electrode_results(rw, electrode_pos = electrode_pos(rw), smoothing_count = 4)

# Shift the time of the results.
res$time_sec <- res$time_sec + min_time

# Make a tall data set.
res_w_src <- cbind(res, src = "simulation")
res_w_src$electrode <- res$electrode + y_base

dat_w_src <- cbind(dat, src = "experiment")

sim_w_dat <- rbind(res_w_src, dat_w_src)

# Work out interpolation.
# The experimental data has the lower resolution
head(dat_w_src[ dat_w_src$time_sec >= min_time, ])

# Add the slope/intercept for each segment.
dat_slp_intcpt <- slope_intercept_df(dat_w_src[ , 1:2])

head(dat_slp_intcpt[ dat_w_src$time_sec >= min_time, ])

# The model has the higher resolution. You'll need its time series
# for interpolating into the experimental data and upscaling it.
head(res_w_src, 100)

# Build a new data frame for upsampled experimental data.
# Time series from the model.
dat_up <- cbind(res_w_src$time_sec)

head(dat_up)
head(dat_slp_intcpt[ dat_w_src$time_sec >= min_time, ])

# Function get_slope_intercept() returns for one time stamp.
# This approach won't go into mapply().
get_slope_intercept(dat_slp_intcpt, dat_up[2, 1])
get_slope_intercept(dat_slp_intcpt, dat_up[4009, 1])

dat_up[1:3, 1]
# See?
mapply(get_slope_intercept, dat_slp_intcpt, dat_up[1:3, 1])

# This returns a data frame of time stamps with slopes and intercepts
head(res_w_src$time_sec)
tail(res_w_src$time_sec)
dat_up_si <- get_slope_intercepts(dat_slp_intcpt, res_w_src$time_sec)

head(dat_slp_intcpt[dat_slp_intcpt > 10, ])
head(dat_up_si)

str(dat_up_si)
tail(dat_slp_intcpt)
tail(dat_up_si)
head(dat_slp_intcpt[dat_slp_intcpt > 10.1, ])
head(dat_up_si[dat_up_si$time_sec > 10.19, ])

m <- dat_up_si$slope
x <- dat_up_si$time_sec
y <- dat_up_si$intercept

interpolate <- m * x + y

dat_up_si <- cbind(dat_up_si, electrode = interpolate)
dat_up_si <- cbind(dat_up_si, src = "interpolation", stringsAsFactors = FALSE)

str(dat_up_si)
plot(dat_up_si$time_sec, dat_up_si$electrode)

head(dat_up_si)
head(sim_w_dat)

sim_dat_intrpol <- rbind(sim_w_dat, dat_up_si[ , c(1, 4, 5)])

# Plot simulation and data
ggplot(data = sim_w_dat) +
        geom_line(mapping = aes(x = time_sec, y = electrode, colour = src)) +
        labs(title = "Cyclic Voltammetry Simulation",
             subtitle = paste("Input Data File: ", fil),
             x = "time [s]",
             y = expression(paste("DA concentration [", mu, "M]")),
             colour = "source") # +
        # annotate("text", x = Inf, y = Inf, label = caption, vjust = 1, hjust = 1)

# Add interpolation to plot
ggplot(data = sim_dat_intrpol) +
        geom_line(mapping = aes(x = time_sec, y = electrode, colour = src)) +
        labs(title = "Cyclic Voltammetry Simulation",
             subtitle = paste("Input Data File: ", fil),
             x = "time [s]",
             y = expression(paste("DA concentration [", mu, "M]")),
             colour = "source") # +
# annotate("text", x = Inf, y = Inf, label = caption, vjust = 1, hjust = 1)

head(sim_dat_intrpol)

sim_dat_intrpol_split <- split(sim_dat_intrpol[ , 1:2], sim_dat_intrpol[3])

str(sim_dat_intrpol_split)
sim_dat_intrpol_split$simulation

cor(sim_dat_intrpol_split$simulation$electrode, sim_dat_intrpol_split$interpolation$electrode)
