# library(dplyr)
# library(ggplot2)
# library(data.table)
# 
# # Global variables
# pipeline_dir <- "/media/sf_OneDrive_-_cumc.columbia.edu/rwalk/pipeline"
# input_dir <- paste(pipeline_dir, "06_Library", sep = "/")# WT - Pre-AMPH
# output_dir <- paste(pipeline_dir, "output", "plots", sep = "/")
# 
# # Constants
# pulses <- 30
# pulse_freq <- 50
# bin_size <- 2.0
# electrode_distance <- 1000
# diffusion_coefficient <- 2.7 * 10^-6
# convert_current <- FALSE
# fit_region = "fall"
# 
# # # Set up a parameters file.
# # # One time only. Then edit it.
# # animals <- stim_df %>%
# #         distinct(animal, genotype) %>%
# #         arrange(animal)
# # stim_bands <- data.frame(matrix(c(1, 3, 6, 10, 16, 20), nrow = 3, byrow = TRUE))
# # # Produce Cartesian product.
# # params <- crossing(animals, stim_bands)
# # names(params)[3:4] <- c("stim_low", "stim_high")
# # params <- cbind(params,
# #                 "release" = as.double(NA),
# #                 "vmax" = as.double(NA),
# #                 "km" = as.double(NA),
# #                 "dead_space_distance" = as.double(NA),
# #                 "base_tolerance" = as.double(NA),
# #                 "plot_duration_sec" = as.integer(NA))
# # write.csv(params, file = paste(input_dir, "dead_space_avg_params.csv", sep = "/"))
# 
# # Read data and plot parameters
# dat_fil <- "stim_df.csv"
# params_fil <- "dead_space_avg_params.csv"
# stim_df <- fread(paste(input_dir, dat_fil, sep = "/"))

params <- fread(paste(input_dir, params_fil, sep = "/"))
# Pick off the complete parameters for plotting.
complete_params <- complete.cases(params)
plot_params <- params[complete_params, ]

#for (i in 1:nrow(plot_params)) {
for (i in 6:6) {
        # Variables
        an_animal <- plot_params$animal[i]
        stim_low <- plot_params$stim_low[i]
        stim_high <- plot_params$stim_high[i]
        release <-plot_params$release[i] 
        vmax <- plot_params$vmax[i]
        km <- plot_params$km[i]
        dead_space_distance <- plot_params$dead_space_distance[i]
        base_tolerance <- plot_params$base_tolerance[i]
        plot_duration_sec <- plot_params$plot_duration_sec[i]        
        
        dat_fit <- select(stim_df, animal, stim_time_sec, electrode_concentration, genotype, stimulus, include) %>%
                filter(animal == an_animal & stimulus >= stim_low & stimulus <= stim_high & include == TRUE & stim_time_sec <= 120) %>%
                group_by(stim_time_sec) %>%
                summarize(mean(electrode_concentration))
        dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = `mean(electrode_concentration)`)
        
        compare_pulse(dat = dat_fit, fil = paste0(an_animal, " ", plot_params$genotype[i],
                                                  " Stim range = ", stim_low, ":", stim_high),
                      vmax = vmax, km = km,
                      pulses = pulses,
                      pulse_freq = pulse_freq,
                      release = release,
                      bin_size = bin_size,
                      electrode_distance = electrode_distance,
                      dead_space_distance = dead_space_distance,
                      diffusion_coefficient = diffusion_coefficient,
                      convert_current = convert_current,
                      fit_region = fit_region,
                      base_tolerance = base_tolerance,
                      plot_duration_sec = plot_duration_sec,
                      dead_space = dead_space_distance)
        
        # Copy the plot from the screen device to the file device.
        fil_name <- paste0(output_dir, "/", an_animal, "_",
                           plot_params$genotype[i], "_stim_",
                           stim_low, "_", stim_high, ".jpg")
        # dev.copy(jpeg, file = fil_name)
        # dev.off() # Don't forget!        
}

# # Review stimuli
# animal_review <- an_animal
# dat_review <- stim_df %>%
#         filter(animal == animal_review & stimulus >= 1 & stimulus <= 15 & include == TRUE)
# qplot(data = dat_review, x = time_sec, y = electrode_concentration, geom = "line")
# stim_review <- 3
# dat_review_stim <- dat_review[dat_review$stimulus == stim_review & dat_review$stim_time_sec < 30, ]
# qplot(data = dat_review_stim, x = stim_time_sec, y = electrode_concentration, geom = "line")
