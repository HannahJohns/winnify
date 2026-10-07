
  this_data <- winnify::tte_longform
  this_data$arm <- factor(this_data$arm,levels = c("B","A"))
  wins(survival::Surv(time,event_1) +
                            survival::Surv(time,event_2) +
                            survival::Surv(time,event_3) ~
                            w_arm(arm) + w_time(time) + w_id(id) + Z1 + Z2,
                          data=this_data
  )

  wins(survival::Surv(time,event_1) +
                            survival::Surv(time,event_2) +
                            survival::Surv(time,event_3) ~
                            w_arm(arm) + w_time(time) + w_id(id) + Z1 + Z2,
                          data=this_data[nrow(this_data):1,]
  )













# #That's horrifying

# WINS::win.stat(data = WINS::data_tte, ep_type = "tte", arm.name = c("A","B"), tau = 0.1,
#          Z_t_trt = WINS::Z_t_trt, Z_t_con = WINS::Z_t_con, priority = c(1:3), alpha = 0.05, digit = 3,
#          method = "ipcw_tau", stratum.weight = "unstratified", pvalue = "two-sided")

WINS::win.stat(data = WINS::data_tte,
         ep_type = "tte", arm.name = c("A","B"), tau = 0.1,
         Z_t_trt = NULL, # WINS::Z_t_trt[order(runif(nrow(WINS::Z_t_trt))),] %>% filter(time==0),
         Z_t_con = NULL, #WINS::Z_t_con[order(runif(nrow(WINS::Z_t_con))),] %>% filter(time==0),
         priority = c(1:3), alpha = 0.05, digit = 3,
         method = "ipcw",
         stratum.weight = "unstratified", pvalue = "two-sided")


WINS::win.stat(data = WINS::data_tte[nrow(WINS::data_tte):1,],
         ep_type = "tte", arm.name = c("A","B"), tau = 0.1,
         Z_t_trt = NULL, # WINS::Z_t_trt[order(runif(nrow(WINS::Z_t_trt))),] %>% filter(time==0),
         Z_t_con = NULL, #WINS::Z_t_con[order(runif(nrow(WINS::Z_t_con))),] %>% filter(time==0),
         priority = c(1:3), alpha = 0.05, digit = 3,
         method = "ipcw",
         stratum.weight = "unstratified", pvalue = "two-sided")






