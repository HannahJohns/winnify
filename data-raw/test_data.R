# Load survival package to use the dataset 'bladder1'
library(survival)
library(tidyverse)

# Creation of dataset 'df' with 3 outcomes:
#   Outcome 1: death (survival event)
#   Outcome 2: cancer recurrence (repeated survival event)
#   Outcome 3: size of largest initial tumour (continuous event)
data1 <- bladder1  %>%
        mutate(trt = if_else(treatment == "placebo", "Placebo", "Treatment")) %>%
        group_by(id) %>%
        mutate(death = if_else(max(status) %in% c(2, 3), 1, 0),
               t2death = max(stop)) %>%
        ungroup() %>%
        select(id, trt, death, t2death, number, size) %>%
        unique()

data2 <- bladder1 %>%
        filter(status == 1) %>%
        select(id, t2recurr = stop) %>%
        mutate(recurr = 1) %>%
        arrange(id, t2recurr) %>%
        group_by(id) %>%
        mutate(nrecurr = row_number()) %>%
        ungroup() %>%
        full_join((data1 %>% select(id)), by = "id") %>%
        complete(id, nrecurr) %>%
        filter(!is.na(nrecurr)) %>%
        full_join((data1 %>% select(id, t2death)), by = "id") %>%
        mutate(
                recurr = replace(recurr, is.na(recurr), 0),
                t2recurr = if_else(is.na(t2recurr), t2death, t2recurr)
        ) %>%
        select(id, nrecurr, recurr, t2recurr)

data3 <- data2 %>%
        pivot_wider(id_cols = "id", names_from =  nrecurr,
                    values_from = recurr, names_prefix  = "recurr")

data4 <- data2 %>%
        pivot_wider(id_cols = "id", names_from =  nrecurr,
                    values_from = t2recurr, names_prefix  = "t2recurr")

data5 <- full_join(data3, data4, by = "id")

data_test <- full_join(data1, data5, by = "id")

usethis::use_data(data_test)
