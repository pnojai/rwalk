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
# # plot_duration_sec = 40
# 
# # Read data and plot parameters
# dat_fil <- "stim_df.csv"
# params_fil <- "dead_space_params.csv"
# stim_df <- fread(paste(input_dir, dat_fil, sep = "/"))

params <- fread(paste(input_dir, params_fil, sep = "/"))
# Pick off the complete parameters for plotting.
complete_params <- complete.cases(params)
plot_params <- params[complete_params, ]

# for (i in 1:nrow(plot_params)) {
for (i in 1:29) {
        # Variables
        an_animal <- plot_params$animal[i]
        a_stim <- plot_params$stimulus[i]
        release <-plot_params$release[i] 
        vmax <- plot_params$vmax[i]
        km <- plot_params$km[i]
        dead_space_distance <- plot_params$dead_space_distance[i]
        base_tolerance <- plot_params$base_tolerance[i]
        plot_duration_sec <- plot_params$plot_duration_sec[i]        
        
        dat_fit <- select(stim_df, animal, stimulus, stim_time_sec, electrode_concentration) %>%
                filter(animal == an_animal & stimulus == a_stim)
        dat_fit <- rename(dat_fit, time_sec = stim_time_sec, "electrode" = electrode_concentration)
        dat_fit <- dat_fit[ , -c(1, 2)]

        compare_pulse(dat = dat_fit, fil = paste(an_animal, plot_params$genotype[i],
                                                  "Stim =", a_stim),
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
        fil_name <- paste0(output_dir, "/", an_animal, "_", plot_params$genotype[i], "_", a_stim, ".jpg")
        # dev.copy(jpeg, file = fil_name)
        # dev.off() # Don't forget!        
}

