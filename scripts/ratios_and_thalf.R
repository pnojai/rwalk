# Assumes merge_files.R already loaded stim_df.

wt_pre_max_concentration <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 20) %>%
        group_by(animal) %>%
        summarize(max(electrode))

wt_post_max_concentration <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 6 & stimulus <= 10 & include == TRUE & stim_time_sec <= 20) %>%
        group_by(animal) %>%
        summarize(max(electrode))

ko_pre_max_concentration <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "ko" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 20) %>%
        group_by(animal) %>%
        summarize(max(electrode))

ko_post_max_concentration <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "ko" & stimulus >= 6 & stimulus <= 10 & include == TRUE & stim_time_sec <= 20) %>%
        group_by(animal) %>%
        summarize(max(electrode))

wt_ratio_pre <- wt_post_max_concentration$`max(electrode)` / wt_pre_max_concentration$`max(electrode)`
ko_ratio_pre <- ko_post_max_concentration$`max(electrode)` / ko_pre_max_concentration$`max(electrode)`

wt_ratio_pre
ko_ratio_pre

sd(wt_ratio_pre)
sd(ko_ratio_pre)

t.test(wt_ratio_pre, ko_ratio_pre, paired = FALSE, var.equal = FALSE)

head(stim_df)

wt_pre_max_concentration

dat_merge_wt_pre <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "wt" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 20) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode))
dat_merge_wt_pre <- rename(dat_merge_wt_pre, time_sec = stim_time_sec, "electrode" = "mean(electrode)")

dat_merge_ko_pre <- select(stim_df, animal, stim_time_sec, electrode, genotype, stimulus, include) %>%
        filter(genotype == "ko" & stimulus >= 1 & stimulus <= 3 & include == TRUE & stim_time_sec <= 20) %>%
        group_by(stim_time_sec) %>%
        summarize(mean(electrode))
dat_merge_ko_pre <- rename(dat_merge_ko_pre, time_sec = stim_time_sec, "electrode" = "mean(electrode)")

wt_pre_avg_max <- max(dat_merge_wt_pre$electrode)
wt_pre_avg_max

wt_pre_avg_max_half <- wt_pre_avg_max / 2.0
wt_pre_avg_max_half

ggplot(data = dat_merge_wt_pre) +
        geom_point(mapping = aes(x = time_sec, y = electrode)) +
        geom_hline(yintercept = wt_pre_avg_max_half)
