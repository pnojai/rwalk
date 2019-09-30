library(dplyr)

stim_df %>% filter(stim_time_sec == 0) %>% arrange(desc(electrode_concentration))
stim_df %>% filter(stim_time_sec == 0, animal == 1905211) 

conc_correction <- stim_df %>% 
        filter(stim_time_sec == 0) %>% 
        mutate(electrode_concentration_correction = -electrode_concentration) %>% 
        select(animal, stimulus, electrode_concentration_correction)

head(conc_correction)

conc_correction[conc_correction$animal == 1905211, ]
