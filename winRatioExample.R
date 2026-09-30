rm(list=ls())

source("R/specials.R")
source("R/wins.R")
source("R/ranks.R")
source("R/as_winnify.R")
source("R/winnify.R")

load("data/neuro_improvement.rda")
data <- neuro_improvement
# data$treat <- factor(data$treat)
formula <- mRS+early_neuro_improvement~w_arm(treat)
ranks_statistic <- pim

x <- winnify(formula, ranks, data, ranks_statistic = pim, exponentiate=T)
plot(x)


library(WINS)
library(survival)

df <- data_tte

df$strata <- rbinom(n=nrow(df),prob = 0.5,size = 1)

df1 <- Z_t_trt
df2 <- Z_t_con

df$id <- paste0(df$arm,df$id)
df1$id <- paste0("A",df1$id)
df2$id <- paste0("B",df2$id)

df$time <- 0

data <- merge(
        rbind(
                df1,
                df2
        ),
        df,
        all.x = T
)


# data$Surv1 <- Surv(data$Y_1,data$Delta_1)
# data$Surv2 <- Surv(data$Y_2,data$Delta_2)
# data$Surv3 <- Surv(data$Y_3,data$Delta_3)

data$arm <- factor(data$arm, levels = c("B","A"))

formula <- Surv(Y_1,Delta_1)+Surv(Y_2,Delta_2)+Surv(Y_3,Delta_3)~w_arm(arm) + w_stratum(strata) + w_id(id) + w_time(time) + Z1 + Z2

# formula <- Surv(Y_1,Delta_1)+Surv(Y_2,Delta_2)+Surv(Y_3,Delta_3)~wins_arm(arm) + wins_stratum(strata) +  Z1 + Z2

source("R/wins.R")

statistic <- wins
wins(formula,data)

x <- winnify(formula,statistic,data, tau=c(1,0.5,0))

plot(x, effect_size="wr")

source("R/winnify.R")

plot(x, effect_size = "wr")
plot(x, effect_size = "wo") # this needs fixed, should be able to mark wo
plot(x, effect_size = "nb")

# tmp <- wins(formula,data)
#
# attr(tmp,"groups")
#
# attr(tmp,"arguments")$formula
# str(tmp)

# formula <- Surv1+Surv2+Surv3~group(arm) + id(id) + time(time) + Z1 + Z2





statistic <- function(formula,data,...){
        fit <- genodds::genodds(response = data[,as.character(formula)[2]],
                                group = data[,as.character(formula)[3]])
        out <- c(fit$pooled_lnodds,fit$pooled_lnconf.int, fit$pooled_p)
        names(out) <- c("estimate","lower","upper","pval")
        out[1:3] <- exp(out[1:3])
        out
}

x <- winnify(mRS+early_neuro_improvement~treat,
             statistic = statistic,
             data = neuro_improvement)
plot(x,neutral_point = 1,log_scale = F)


load("data/remapcap.rda")

# Calculate time at home as time post discharge
remapcap$id <- 1:nrow(remapcap)

remapcap$homeTime <- (remapcap$deathTime-remapcap$dischargeTime)

# Collapse treatment groups so only one treatment group
remapcap$group <- ifelse(remapcap$group == "uc", "Control", "Treatment")

# Run winratio function on dataset
wr1 <- WinRatio::winratio(id = "id", trt = "group", active = "Treatment",
                outcomes = list("Death" = c("deathStatus", "s", "deathTime"),
                                "Time at home" = c("homeTime", "c", ">")),
                fu = "deathTime", data = remapcap)

x <- wr1

summary(wr1)
plot(as_winnify(wr1))

remapcap

library(WINS)

statistic <- function(formula,data,...){


        this_data <- data.frame()



        fit <- genodds::genodds(response = data[,as.character(formula)[2]],
                                group = data[,as.character(formula)[3]])
        out <- c(fit$pooled_lnodds,fit$pooled_lnconf.int, fit$pooled_p)
        names(out) <- c("estimate","lower","upper","pval")
        out[1:3] <- exp(out[1:3])
        out
}



# as_winnify.data.frame test

# Create test dataframe for plot

# Calculate time at home as time post discharge
remapcap$homeTime <- (remapcap$deathTime-remapcap$dischargeTime)

# Collapse treatment groups so only one treatment group
remapcap$group <- ifelse(remapcap$group == "uc", "Control", "Treatment")
remapcap$id <- 1:nrow(remapcap)


library(WinRatio)
# Run winratio function on dataset
wr1 <- winratio(id = "id", trt = "group", active = "Treatment",
                outcomes = list("Death" = c("deathStatus", "s", "deathTime"),
                "Time at home" = c("homeTime", "c", "<")),
                fu = "deathTime", data = remapcap)

# Plot win ratio results
tmp <- as_winnify.WinRatio(wr1)

plot(tmp)