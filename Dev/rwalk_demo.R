# Build matrices for bins of length: 2.0, 1.0, .5
rw_bin20 <- rwalk_amp(dead_space_distance = 4)
rw_bin10 <- rwalk_amp(dead_space_distance = 4, bin_size = 1.0)
rw_bin05 <- rwalk_amp(dead_space_distance = 4, bin_size = .5)

# Where is the electrode?
electrode_pos(rw_bin20)
electrode_pos(rw_bin10)
electrode_pos(rw_bin05)

# How about just releasing at the end?
# rw_bin20 <- rwalk_amp(electrode_distance = 50, dead_space_distance = 48)
# rw_bin10 <- rwalk_amp(electrode_distance = 50, dead_space_distance = 49, bin_size = 1.0)
# rw_bin05 <- rwalk_amp(electrode_distance = 50, dead_space_distance = 49.5, bin_size = .5)

# Each object is a data frame. The time series is in the row names.
str(rw_bin20)

# Assemble results
res_bin20 <- electrode_results(rw_bin20, electrode_pos = electrode_pos(rw_bin20))
res_bin10 <- electrode_results(rw_bin10, electrode_pos = electrode_pos(rw_bin10))
res_bin05 <- electrode_results(rw_bin05, electrode_pos = electrode_pos(rw_bin05))

# Plot
plot(row.names(res_bin20), res_bin20$electrode, type = "l")
plot(row.names(res_bin10), res_bin10$electrode, type = "l")
plot(row.names(res_bin05), res_bin05$electrode, type = "l")

# Examine electrode at beginning of time series.
head(res_bin20)
head(res_bin10)
head(res_bin05)

# Examine time .2
# res_df_bin20[res_df_bin20[,1] == .2]
# res_df_bin10[res_df_bin10[,1] == .2]
# res_df_bin05[res_df_bin05[,1] == .2]

# Look at some experimental data. Read it.
dat <- read_experiment_csv("Data/AMPH_test.csv")
# Plot it
plot(row.names(dat), dat[ ,1], type = "l")

dat_trim <- trim_results(dat)
#Plot
plot(row.names(dat_trim), dat_trim[ ,1], type = "l")


