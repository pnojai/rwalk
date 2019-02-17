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

tail(dat_up)
tail(dat_slp_intcpt[ dat_w_src$time_sec >= min_time, ])

# Function get_slope_intercept() returns for one time stamp.
get_slope_intercept(dat_slp_intcpt, dat_up[2, 1])
get_slope_intercept(dat_slp_intcpt, dat_up[4009, 1])



