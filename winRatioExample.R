library(WINS)

?win.stat


# Taken from COMPARE WINS
# https://github.com/HannahJohns/compare-wins/blob/main/source/app/shiny/R/misc_functions.R

wins <- function(formula, data, ...){

}

# WINS::win.stat
wins_wrapper <- function(data, outcomes, arm, levels,
                         preference_method="heirarchical",
                         stratum=NULL,
                         covariates=NULL,
                         method = "unadjusted",
                         stratum.weight = "MH-type",
                         pvalue = "two-sided",
                         alpha=0.05
){

        # First, we need to convert the data sheet into the correct format

        formatted_data <- data.frame(id=1:nrow(data))
        formatted_data$arm <- factor(data[,arm])

        if(length(stratum)>0){
                if(length(stratum)==1){
                        formatted_data$stratum <- data[,stratum]
                } else {
                        formatted_data$stratum <- do.call("paste", lapply(stratum, function(x){data[,x]}))
                }
        } else {
                stratum.weight <- "unstratified"
        }

        if(preference_method=="heirarchical"){

                ep_type <- {}
                np_direction <- {}
                tau <- {}
                for(i in 1:length(outcomes)){
                        ep_type[i] <- outcomes[[i]]$type
                        tau[i] <- outcomes[[i]]$tau
                        np_direction[i] <- c(`>`="larger",`<`="smaller")[outcomes[[i]]$direction]

                        formatted_data[,sprintf("Y_%d",i)] <- data[,outcomes[[i]]$var]
                        if(ep_type[i] %in% c("surv","tte")){
                                ep_type[i] <- "tte"
                                formatted_data[,sprintf("Delta_%d",i)]  <- data[,outcomes[[i]]$indicator]

                                if(length(
                                        setdiff(
                                                unique(formatted_data[,sprintf("Delta_%d",i)]),
                                                0:1
                                        )
                                ) >0 ){

                                        warning(sprintf(
                                                "%s includes values other than 1 and 0. All values not 1 assumed censored.",
                                                outcomes[[i]]$var
                                        ), stderr())

                                        formatted_data[,sprintf("Delta_%d",i)] <- ifelse(formatted_data[,sprintf("Delta_%d",i)]==1,1,0)
                                }


                        }
                }

        } else {
                stop("NON-HEIRARCHICAL METHODS NOT IMPLEMENTED YET")
        }


        # Only support covariates at baseline for the time being.

        # NOTE: This needs added
        if(length(covariates)==0){
                Z_t_trt <- Z_t_con <- NULL
        } else {
                # At present only support covariates at baseline
                Z_t_con <- data.frame(id=1:nrow(formatted_data), time=0)
                for(i in 1:length(covariates)){
                        Z_t_con[,covariates[i]] <- data[,covariates[i]]
                }
                Z_t_trt <-Z_t_con

                # Z_t_trt/con must contain only controls and treatments respectively
                Z_t_trt <- Z_t_trt[which(formatted_data$arm==levels[1]),]
                Z_t_con <- Z_t_con[which(formatted_data$arm==levels[2]),]
        }

        out <- WINS::win.stat(data = formatted_data,
                              ep_type = ep_type,
                              priority = 1:length(outcomes),
                              arm.name = levels,
                              tau = tau,
                              np_direction = np_direction,
                              Z_t_trt = Z_t_trt,
                              Z_t_con = Z_t_con,
                              method = method,
                              stratum.weight = stratum.weight,
                              pvalue = pvalue,
                              alpha=alpha,
                              summary.print=FALSE
        )

        # Format results into a table to report back

        estimates <- do.call("rbind",lapply(names(out$Win_statistic),function(i){
                cbind(outcome=i,
                      as.data.frame(matrix(out$Win_statistic[[i]],nrow=1))
                )
        }))

        colnames(estimates)[-1] <- c("estimate","lower","upper")

        estimates$p <- c(out$p_value)

        # Win/loss/tie proportions only make sense to report if unstratified
        if(length(stratum)==0){
                estimates$win <- out$Win_prop$P_trt
                estimates$loss <- out$Win_prop$P_con
                estimates$tie <- 1 - (out$Win_prop$P_trt + out$Win_prop$P_con)

        } else {

                #TODO: Get wins/losses/ties in stratified analysis.
                # This can be manually extracted.

                # N is sample size per strata
                # N_event is number of events per arm.
                # Based on the following code:

                # https://github.com/cran/WINS/blob/master/R/unadjusted.win.stat.R#L146

                # It looks like number of events is based on if there is
                # at least one event on any component, i.e. we fall back to composite
                # tte for estimating these weights.


                xtab <- table(
                        formatted_data$stratum,
                        formatted_data$arm
                )

                N = apply(xtab,1,sum)

                total_pairs <- apply(xtab,1,prod)

                if(any(ep_type=="tte")){
                        ind.tte <- which(ep_type == "tte")
                        n_tte <- length(ind.tte)

                        N_event <- apply(as.matrix(
                                formatted_data[,sprintf("Delta_%d",ind.tte)],
                                ncol = n_tte), 1, function(x) max(x)>0)

                        N_event <- tapply(N_event,formatted_data$stratum,sum)

                } else {
                        N_event=N
                }


                w_stratum = switch(stratum.weight,
                                   "unstratified" = 1,
                                   "equal" = rep(1/length(N),length(N)),
                                   "MH-type" = (1/N)/sum(1/N),
                                   "wt.stratum1" = N/sum(N),
                                   "wt.stratum2" = N_event/sum(N_event)
                )



                wins <- out$Win_prop[,"P_trt"]*total_pairs
                losses <- out$Win_prop[,"P_con"]*total_pairs
                ties <- total_pairs - wins - losses


                estimates$win <- sum(w_stratum*wins)
                estimates$loss <- sum(w_stratum*losses)
                estimates$tie <- sum(w_stratum*ties)

                total_pairs <- estimates$win + estimates$loss +  estimates$tie

                estimates$win  <-  estimates$win/total_pairs
                estimates$loss  <-  estimates$loss/total_pairs
                estimates$tie  <-  estimates$tie/total_pairs

        }

        estimates

}








plot(result, pairs.fill = "RdBu",effect_neutral_point = 1)

#winnify() test

rm(list = ls())

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

formula <-




library(WinRatio)
# Run winratio function on dataset
wr1 <- winratio(id = "id", trt = "group", active = "Treatment",
                outcomes = list("Death" = c("deathStatus", "s", "deathTime"),
                "Time at home" = c("homeTime", "c", "<")),
                fu = "deathTime", data = remapcap)

# Plot win ratio results
tmp <- as_winnify.WinRatio(wr1)

plot(tmp)

# library(WinRatio)
# library(WinRatioPlot)
# ?WinRatio::winratio

#########################################################################################

rm(list=ls())

# data_test_remapcap <- read.csv("generated_data.csv")

data_test_remapcap <- data_remapcap
data_test_remapcap$homeTime <- (21-data_test_remapcap$dischargeTime)
data_test_remapcap$group <- ifelse(data_test_remapcap$group == "uc", "Control", "Treatment")

wr_remapcap <- WinRatio::winratio(id = "X", trt = "group", active = "Treatment",
                         outcomes = list("Death" = c("deathStatus", "s", "deathTime"),
                                         "Time to discharge" = c("dischargeStatus", "s", "dischargeTime"),
                                         "Organ support-free days" = c("score", "c", ">")),
                         fu = "deathTime", data = data_test_remapcap)

WinRatioPlot::plot.WinRatio(wr_remapcap)

######################################################################################
data_test <- read.csv("test_data.csv")

wr <- WinRatio::winratio(id = "id", trt = "trt", active = "Treatment",
outcomes = list("Death" = c("death", "s", "t2death"),
                "Length of hospital stay" = list(paste0("recurr", 1:9), "r", paste0("t2recurr", 1:9)),
                "Wellbeing at 28 days" = c("size", "c", ">")),
fu = "t2death", data = data_test)

WinRatioPlot::plot.WinRatio(wr)

ggp <- WinRatioPlot::plot.WinRatio(wr)

class(ggp)




WinRatioPlot::winRatioPlot(test_data_overall_fn, effect_name = "Win ratio", neutral_point = 1,
                           outcome_names="outcome_names", win_inherit="win_inherit", win="win", tie="tie",
                           loss="loss", loss_inherit="loss_inherit",
                           win_ratio_individual="win_ratio", ci_lower_individual="ci_lower", ci_upper_individual="ci_upper",
                           win_ratio_cumulative="win_ratio_cum", ci_lower_cumulative="ci_lower_cum", ci_upper_cumulative="ci_upper_cum")

test_data_overall_fn2 <- subset(test_data_overall_fn, select = -c(outcome_names))

WinRatioPlot::winRatioPlot(test_data_overall_fn2, effect_name = "Odds ratio", neutral_point = 2, tie_handling="drop",
                           outcome_names="outcome_names", win_inherit="win_inherit", win="win", tie="tie",
                           loss="loss", loss_inherit="loss_inherit",
                           win_ratio_individual="win_ratio", ci_lower_individual="ci_lower", ci_upper_individual="ci_upper",
                           win_ratio_cumulative="win_ratio_cum", ci_lower_cumulative="ci_lower_cum", ci_upper_cumulative="ci_upper_cum")




##############################################
# summary(wr)
ggp <- WinRatioPlot::plot.WinRatio(wr)

ggp[[1]]
ggp[[2]]

summary(ggp)
str(ggp)


cowplot::get_legend(ggp[[1]])

ggarrange(
        ggp[[1]],
        ggp[[2]],common.legend = F
)
ggp_legend_1 <- get_legend(ggp[[1]])
ggp_legend_2 <- get_legend(ggp[[2]])



cowplot::plot_grid(ggp[[1]]+theme(legend.position = "none"),
                   ggp[[2]]+theme(legend.position = "none"),
                   NULL,
                   plot_grid(
                           ggp_legend_2+theme(legend.text = element_text(18)),
                           NULL,
                           ggp_legend_1,
                           ncol = 1,rel_heights=c(1,-0.6,1),axis="l"
                   ),
                   nrow = 1,
                   rel_widths = c(1,1,-1,1)
                   )

ggarrange(
        ggp[[1]],
        ggp[[2]],common.legend = F
)


ggplot()+
        xlim(0,10)+
        annotate(geom="rect",xmin=0,xmax=1,ymin=0,ymax=1,fill="dark red")+
        annotate(geom="text",x=1,y=0.5,label="Inherited losses",hjust=0)+
        annotate(geom="rect",xmin=0,xmax=1,ymin=1,ymax=2,fill="red")+

        annotate(geom="segment",x=0,xend=1,y=5,yend=5)+
        annotate(geom="point",x=0.5,y=5,size=5)

