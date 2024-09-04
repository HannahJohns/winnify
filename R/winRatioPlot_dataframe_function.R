
# Use a name like plot.x - what we're doing is producing what's called an S3 generic function
# for the winratio plot.
# https://adv-r.hadley.nz/s3.html

# Win ratio plot function
plot.WinRatio <- function(wr)
{
        library(WinRatio)

        # Extract data and create dataframe
        wr_output_list <- lapply(1:length(wr$call$outcomes), function(i) {

                # Get estimates for win ratio based on outcome i and outcomes 1 to i
                tmp_wr_individual <- winratio(id = wr$call$id, trt = wr$call$trt, active = wr$call$active,
                                              outcomes = wr$call$outcomes[i],
                                              fu = wr$call$fu, data = wr$call$data)

                tmp_wr_cumulative <- winratio(id = wr$call$id, trt = wr$call$trt, active = wr$call$active,
                                outcomes = wr$call$outcomes[1:i],
                                fu = wr$call$fu, data = wr$call$data)

                data.frame(level=i, level_names=names(tmp_wr_cumulative$call$outcome[i]),
                           win_inherit=(tmp_wr_cumulative$total.wins-tmp_wr_cumulative$wins[i]), win=tmp_wr_cumulative$wins[i],
                           tie=tmp_wr_cumulative$total.ties,
                           loss=tmp_wr_cumulative$loss[i], loss_inherit=(tmp_wr_cumulative$total.loss-tmp_wr_cumulative$loss[i]),
                           win_ratio=(tmp_wr_cumulative$wins[i]/tmp_wr_cumulative$loss[i]),
                           ci_lower=tmp_wr_individual$wr.lower, ci_upper=tmp_wr_individual$wr.upper,
                           win_ratio_cum=tmp_wr_cumulative$wr,
                           ci_lower_cum=tmp_wr_cumulative$wr.lower, ci_upper_cum=tmp_wr_cumulative$wr.upper
                           )
        }
        )

        wr_dataframe <- do.call("rbind",wr_output_list)

        # Create a dataframe with overall data line info
        overall_line <- data.frame(level=(length(wr$call$outcomes)+1), level_names="Overall", win_inherit=0, win=wr$total.wins,
                                   tie=wr$total.ties, loss=wr$total.loss, loss_inherit=0,
                                   win_ratio=wr$wr, ci_lower=wr$wr.lower, ci_upper=wr$wr.upper,
                                   win_ratio_cum=wr$wr, ci_lower_cum=wr$wr.lower, ci_upper_cum=wr$wr.upper)

        df <- rbind(wr_dataframe, overall_line)


        # TODO: You shouldn't need to write plotting code here!
        # Once you have your data frame in the appropriate format, just run the following

        # WinRatioPlot::winRatioPlot(df)

        # and end the function. This will mean that you only have one copy of the plotting code
        # that needs to be maintained, and if we want to make improvements to the plot we just do it there.

        # It also means that if there was another package we wanted to add plotting ability for, we just have to write
        # a new function for it like what we've done here here.

}


