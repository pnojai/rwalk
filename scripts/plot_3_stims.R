library(dplyr)
library(ggplot2)

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
               # animal %in% c(1904041),
               # animal %in% c(1905211),
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

# fil <- "1904041_F1_Gko_DC10_DNamph4_CUR8500_CON5_DAT.csv"
fil <- "1905211_F1_Gko_DC10_DNamph4_CUR6800_CON5_DAT.csv"
fil_dat <- read.csv(paste(input_dir, fil, sep = "/"))
#fil_dat <- fil_dat[1:12000, ]
qplot(x = seq_along(fil_dat[,1]), y = fil_dat[, 1], geom = "line")
