# Build matrices for bins of length: 2.0, 1.0, .5
rw_bin20 <- rwalk_amp(dead_space_distance = 0)
rw_bin10 <- rwalk_amp(dead_space_distance = 0, bin_size = 1.0)
rw_bin05 <- rwalk_amp(dead_space_distance = 0, bin_size = .5)

# How about just releasing at the end?
rw_bin20 <- rwalk_amp(electrode_distance = 50, dead_space_distance = 48)
rw_bin10 <- rwalk_amp(electrode_distance = 50, dead_space_distance = 49, bin_size = 1.0)
rw_bin05 <- rwalk_amp(electrode_distance = 50, dead_space_distance = 49.5, bin_size = .5)


# Each object is a list. Element 1 is the matrix, element 2 is the time series.
str(rw_bin20)

# Assemble results
res_bin20 <- diffuse(rw_bin20[[1]], electrode_pos = electrode_pos(rw_bin20[[1]]), smoothing_count = 1)
res_bin10 <- diffuse(rw_bin10[[1]], electrode_pos = electrode_pos(rw_bin10[[1]]), smoothing_count = 1)
res_bin05 <- diffuse(rw_bin05[[1]], electrode_pos = electrode_pos(rw_bin05[[1]]), smoothing_count = 1)

# Plot
plot(rw_bin20[[2]][-1], res_bin20, type = "l")
plot(rw_bin10[[2]][-1], res_bin10, type = "l")
plot(rw_bin05[[2]][-1], res_bin05, type = "l")

# Tabulate the observation times and the results
res_df_bin20 <- cbind(rw_bin20[[2]][-1], res_bin20)
res_df_bin10 <- cbind(rw_bin10[[2]][-1], res_bin10)
res_df_bin05 <- cbind(rw_bin05[[2]][-1], res_bin05)

# Examine electrode at beginning of time series.
head(res_df_bin20)
head(res_df_bin10)
head(res_df_bin05)

# Examine time .2
res_df_bin20[res_df_bin20[,1] == .2]
res_df_bin10[res_df_bin10[,1] == .2]
res_df_bin05[res_df_bin05[,1] == .2]
