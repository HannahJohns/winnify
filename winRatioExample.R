
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


# Create test dataframe for plot
test_data_overall_fn <- data.frame(outcome_names=c("Death", "Length of hospital stay", "Time off work", "Wellbeing"),
                                   win_inherit=c(0,15,25,33),
                                   win=c(15,10,8,15),
                                   tie=c(80,60,40,15),
                                   loss=c(5,10,12,10),
                                   loss_inherit=c(0,5,15,27),
                                   win_ratio=c(3,1,0.67,1.5),
                                   ci_lower=c(2.6,0.6,0.27,1.1),
                                   ci_upper=c(3.4,1.4,1.07,1.9),
                                   win_ratio_cum=c(3,1.67,1.22,1.30),
                                   ci_lower_cum=c(2.6,1.32,0.92,1.10),
                                   ci_upper_cum=c(3.4,2.02,1.52,1.50))



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

