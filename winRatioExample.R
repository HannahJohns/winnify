
library(WinRatio)
library(WinRatioPlot)
?WinRatio::winratio

#########################################################################################
data_test <- read.csv("test_data.csv")

wr <- winratio(id = "id", trt = "trt", active = "Treatment",
               outcomes = list(outc1 = c("death", "s", "t2death"),
                               outc2 = list(paste0("recurr", 1:9), "r", paste0("t2recurr", 1:9)),
                               outc3 = c("size", "c", ">")),
               fu = "t2death", data = data_test)

summary(wr)
plot.WinRatio(wr)

str(ggp)

# ggp[[1]]

#########################################################################################

# Load survival package to use the dataset 'bladder1'
# library(survival)
# Creation of dataset 'df' with 3 outcomes:
#   Outcome 1: death (survival event)
#   Outcome 2: cancer recurrence (repeated survival event)
#   Outcome 3: size of largest initial tumour (continuous event)
# data1 <- bladder1  %>%
#         mutate(trt = if_else(treatment == "placebo", "Placebo", "Treatment")) %>%
#         group_by(id) %>%
#         mutate(death = if_else(max(status) %in% c(2, 3), 1, 0),
#                t2death = max(stop)) %>%
#         ungroup() %>%
#         select(id, trt, death, t2death, number, size) %>%
#         unique()
#
# data2 <- bladder1 %>%
#         filter(status == 1) %>%
#         select(id, t2recurr = stop) %>%
#         mutate(recurr = 1) %>%
#         arrange(id, t2recurr) %>%
#         group_by(id) %>%
#         mutate(nrecurr = row_number()) %>%
#         ungroup() %>%
#         full_join((data1 %>% select(id)), by = "id") %>%
#         complete(id, nrecurr) %>%
#         filter(!is.na(nrecurr)) %>%
#         full_join((data1 %>% select(id, t2death)), by = "id") %>%
#         mutate(
#                 recurr = replace(recurr, is.na(recurr), 0),
#                 t2recurr = if_else(is.na(t2recurr), t2death, t2recurr)
#         ) %>%
#         select(id, nrecurr, recurr, t2recurr)
#
# data3 <- data2 %>%
#         pivot_wider(id_cols = "id", names_from =  nrecurr,
#                     values_from = recurr, names_prefix  = "recurr")
#
# data4 <- data2 %>%
#         pivot_wider(id_cols = "id", names_from =  nrecurr,
#                     values_from = t2recurr, names_prefix  = "t2recurr")
#
# data5 <- full_join(data3, data4, by = "id")
#
# df <- full_join(data1, data5, by = "id")

# write.csv(df, "test_data.csv", row.names = FALSE, quote = FALSE)
#
# df <- read.csv("test_data.csv")
#
# # Calculate the win ratio
# wr <- winratio(id = "id", trt = "trt", active = "Treatment",
#                outcomes = list(outc1 = c("death", "s", "t2death"),
#                                outc2 = list(paste0("recurr", 1:9), "r", paste0("t2recurr", 1:9)),
#                                outc3 = c("size", "c", ">")),
#                fu = "t2death", data = df)
#
#
# #Loop iter value
# i <-2
# summary(wr)
#
# # TEST DATAFRAME CODE
# wr_output_full = data.frame()
# for (i in 1:length(wr$call$outcomes)) {
#         wr2 <- winratio(id = wr$call$id, trt = wr$call$trt, active = wr$call$active,
#                         outcomes = wr$call$outcomes[1:i],
#                         fu = wr$call$fu, data = wr$call$data)
#        wr_output_df <- data.frame(level=i, level_names=wr2$call$outcome$outc1[i], win_inherit=(wr2$total.wins-wr2$wins[i]), win=wr2$wins[i],
#                           tie=wr2$total.ties, loss=wr2$loss[i], loss_inherit=(wr2$total.loss-wr2$loss[i]),
#                           win_ratio=(wr2$wins[i]/wr2$loss[i]), ci_lower=2, ci_upper=4,
#                           win_ratio_cum=wr2$wr, ci_lower_cum=wr2$wr.lower, ci_upper_cum=wr2$wr.upper)
#         wr_output_full <- rbind(wr_output_full, wr_output_df)
# }
#
# wr_output_full
#
# wrtest <- winratio(id = wr$call$id, trt = wr$call$trt, active = wr$call$active,
#                 outcomes = wr$call$outcomes[3],
#                 fu = wr$call$fu, data = wr$call$data)
#
#
# # wr_dataframe <- do.call("rbind",wr_output_list)
#
#
# wr_output_list2 <- lapply(1:length(wr$call$outcomes), function(i) {
#         wr2 <- winratio(id = wr$call$id, trt = wr$call$trt, active = wr$call$active,
#                         outcomes = wr$call$outcomes[1:i],
#                         fu = wr$call$fu, data = wr$call$data)
#         data.frame(level=i, level_names=wr2$call$outcome$outc1[i], win_inherit=(wr2$total.wins-wr2$wins[i]), win=wr2$wins[i],
#                    tie=wr2$total.ties, loss=wr2$loss[i], loss_inherit=(wr2$total.loss-wr2$loss[i]),
#                    win_ratio=(wr2$wins[i]/wr2$loss[i]), ci_lower=2, ci_upper=4,
#                    win_ratio_cum=wr2$wr, ci_lower_cum=wr2$wr.lower, ci_upper_cum=wr2$wr.upper)
#         }
#         )
# wr_dataframe <- do.call("rbind",wr_output_list2)
#
#
# wr2 <- winratio(id = wr$call$id, trt = wr$call$trt, active = wr$call$active,
#                 outcomes = wr$call$outcomes[1:i],
#                 fu = wr$call$fu, data = wr$call$data)
# summary(wr2)
#
# wr$total.wins
#
#
# list_of_results <- lapply(1:5, function(i){
#         data.frame(i=i,x=i+1,y=i^2)
# })
#
#
# do.call("rbind",list_of_results)
# # rbind(list_of_results[[1]],list_of_results[[2]],........)
#
# list_of_results <- lapply(1:5, function(i){
#         i
# })
# do.call("c",list_of_results)
#
# wr$call$data
# wr$call$outcomes
#
#
# wr$call$outcomes[1:2]
#
# out1
# out2
# out3
#
# out1+out2
# out1+out2+out3
#
# wr$wins
#
# 1:length(wr$call$outcomes)
#
# lapply(1:5, function(i){
#         list(i=i, cum_i = 1:i)
# })
