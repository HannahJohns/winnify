# Reformatted data from the WINS package
library(tidyverse)

rm(list=ls())
df <- WINS::data_tte
df1 <- WINS::Z_t_trt
df2 <- WINS::Z_t_con

rbind(
  df %>% mutate(Z1=NA, Z2=NA, event_2=NA, event_3=NA) %>% select(id,arm,time=Y_1,Z1,Z2,event_1=Delta_1, event_2, event_3),
  df %>% mutate(Z1=NA, Z2=NA, event_1=NA, event_3=NA) %>% select(id,arm,time=Y_2,Z1,Z2,event_1, event_2=Delta_2, event_3),
  df %>% mutate(Z1=NA, Z2=NA, event_1=NA, event_2=NA) %>% select(id,arm,time=Y_3,Z1,Z2,event_1, event_2, event_3=Delta_3)
) %>%
  rbind(., df1 %>% mutate(arm = "A", event_1=NA,event_2=NA,event_3=NA) %>% select(id,arm,time=time,Z1,Z2,event_1, event_2, event_3)) %>%
  rbind(., df2 %>% mutate(arm = "B", event_1=NA,event_2=NA,event_3=NA) %>% select(id,arm,time=time,Z1,Z2,event_1, event_2, event_3)) %>%
  arrange(arm, id,time) %>%
  mutate(strata = id %%2 ) %>%
  mutate(id = paste0(arm,id)) -> tte_longform

usethis::use_data(tte_longform, overwrite = T)


df %>%
  mutate(strata = id %%2 ) %>%
  mutate(id = paste0(arm,id)) -> tte_outcomes
rbind(
  df1 %>% mutate(arm = "A"),
  df2 %>% mutate(arm = "B")
) %>%
  mutate(strata = id %%2 ) %>%
  mutate(id = paste0(arm,id)) -> tte_covariates

usethis::use_data(tte_outcomes, overwrite = T)
usethis::use_data(tte_covariates, overwrite = T)
