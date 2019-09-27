library(dplyr)

dat <- stim_df %>% 
        filter(between(stimulus, 1, 3),
               between(stim_time_sec, 0, 5),
               include == TRUE,
               genotype == "wt")
dat$animal <- factor(dat$animal)
dat$stimulus <- factor(dat$stimulus)
dat$genotype <- factor(dat$genotype)

ggplot(dat) +
        geom_line(aes(x = stim_time_sec, y = electrode_concentration,
                      color = animal)) +
        labs(title = "Wild Type: Pre-AMPH Stimuli") +
        facet_grid(. ~ stimulus)

dat <- stim_df %>% 
        filter(between(stimulus, 1, 3),
               between(stim_time_sec, 0, 5),
               include == TRUE,
               genotype == "ko")
dat$animal <- factor(dat$animal)
dat$stimulus <- factor(dat$stimulus)
dat$genotype <- factor(dat$genotype)

ggplot(dat) +
        geom_line(aes(x = stim_time_sec, y = electrode_concentration,
                      color = animal)) +
        labs(title = "Knock Out: Pre-AMPH Stimuli") +
        facet_grid(. ~ stimulus)
