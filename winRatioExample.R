
rm(list=ls())



library(WINS)
library(survival)

# Using time-to-event data from the WINS package.
# winnify() expects a single data frame.
 
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
        all.x = TRUE
)

## Use of wins() by itself 

wins(formula,data)












# This needs written still

library(genodds)

my_statistic <- function(formula,data,...){
        lhs <- model.frame(formula[1:2],data=data)
        rhs <- model.frame(formula[c(1,3)],data=data)
  
        if(ncol(lhs)!=1) stop("Left hand side of formula should have one term")
        if(ncol(rhs)!=1) stop("Left hand side of formula should have one term")
  
        fit <- genodds(response = lhs[,1],
                       group = rhs[,1],
                        ...)
  
        # genodds returns an object of class Genodds.
        # if it didn't, we would need to give it a class
        # we can write write S3 methods for.         
        # class(fit) <- c("MyRankStat",class(fit))   
        fit
}

# There's no coef() or confint() functions for
# class Genodds. We need to create some S3 methods to
# extract this info.
coef.Genodds <- function(x){x$pooled_lnodds}
confint.Genodds <- function(x){matrix(x$pooled_lnconf.int,nrow=1)}











source("R/specials.R")
source("R/wins.R")
source("R/ranks.R")
source("R/as_winnify.R")
source("R/winnify.R")

load("data/neuro_improvement.rda")

library(pim)
data <- neuro_improvement
formula <- mRS+early_neuro_improvement~w_arm(treat)

ranks(formula,data,ranks_statistic = pim)




x <- winnify(formula, ranks, data, ranks_statistic = pim, exponentiate=T)
plot(x,effect_neutral_point = 1,pairs_tie = "split")

my_statistic <- function(formula,data,...){
        lhs <- model.frame(formula[1:2],data=data)
        rhs <- model.frame(formula[c(1,3)],data=data)
  
        if(ncol(lhs)!=1) stop("Left hand side of formula should have one term")
        if(ncol(rhs)!=1) stop("Left hand side of formula should have one term")
  
        fit <- genodds::genodds(response = lhs[,1],
                                group = rhs[,1],
                        ...)
  
        # genodds returns an object of class Genodds.
        # if it didn't, we need to
        # give it a class we can write S3 methods
        # for.  
        
        # class(fit) <- c("MyRankStat",class(fit))   
  
        fit
}

# There's no coef() or confint() functions for
# class Genodds. We need to create some S3 methods to
# extract this info.
coef.Genodds <- function(x){x$pooled_lnodds}
confint.Genodds <- function(x){matrix(x$pooled_lnconf.int,nrow=1)}

wo_result <- winnify(formula, ranks, data, ranks_statistic = my_statistic,
                     exponentiate=T, ties="split")
plot(wo_result,effect_neutral_point = 1,pairs_tie = "split")

wr_result <- winnify(formula, ranks, data, ranks_statistic = my_statistic,
                     exponentiate=T, ties="drop")
plot(wr_result,effect_neutral_point = 1,pairs_tie = "drop")


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


wins(formula,data)
x <- winnify(formula,wins,data, tau=c(1,0.5,0))
plot(x, effect_size = "wr")
plot(x, effect_size = "wo")
plot(x, effect_size = "nb")

# tmp <- wins(formula,data)
#
# attr(tmp,"groups")
#
# attr(tmp,"arguments")$formula
# str(tmp)

# formula <- Surv1+Surv2+Surv3~group(arm) + id(id) + time(time) + Z1 + Z2





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