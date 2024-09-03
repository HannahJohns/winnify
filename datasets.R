





# Create dataframe for plot with overall line
test_data_overall_fn <- data.frame(level=1:5,
                                   level_names=c("Death", "Length of hospital stay", "Time off work", "Wellbeing", "Overall"),
                                   win_inherit=c(0,15,25,33,0),
                                   win=c(15,10,8,15,48),
                                   tie=c(80,60,40,15,15),
                                   loss=c(5,10,12,10,37),
                                   loss_inherit=c(0,5,15,27,0),
                                   win_ratio=c(3,1,0.67,1.5,1.30),
                                   ci_lower=c(2.6,0.6,0.27,1.1,1.10),
                                   ci_upper=c(3.4,1.4,1.07,1.9,1.50),
                                   win_ratio_cum=c(3,1.67,1.22,1.30,1.30),
                                   ci_lower_cum=c(2.6,1.32,0.92,1.10,1.10),
                                   ci_upper_cum=c(3.4,2.02,1.52,1.50,1.50))

test_data_overall_fnV2 <- data.frame(level=1:6,
                                     level_names=c("Death", "Length of hospital stay", "Time off work", "Wellbeing", "Independence", "Overall"),
                                     win_inherit=c(0,15,25,33,48,0),
                                     win=c(15,10,8,15,8,56),
                                     tie=c(80,60,40,15,5,5),
                                     loss=c(5,10,12,10,2,39),
                                     loss_inherit=c(0,5,15,27,37,0),
                                     win_ratio=c(3,1,0.67,1.5,4.0,1.44),
                                     ci_lower=c(2.6,0.6,0.27,1.1,3.6,1.24),
                                     ci_upper=c(3.4,1.4,1.07,1.9,4.4,1.64),
                                     win_ratio_cum=c(3,1.67,1.22,1.30,1.44,1.44),
                                     ci_lower_cum=c(2.6,1.32,0.92,1.10,1.24,1.24),
                                     ci_upper_cum=c(3.4,2.02,1.52,1.50,1.64,1.64))




# Test with data set
wrplotfn_colourpanel_reorder(test_data_overall_fn)

# Test with different data set with extra level
wrplotfn_colourpanel_reorder(test_data_overall_fnV2)
