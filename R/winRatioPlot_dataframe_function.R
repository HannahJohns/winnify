#' Win ratio plot function
#'
#' This is an extended explanation of what this function does
#'
#' @usage
#' fun(arg1, arg2 = default, ...)
#'
#' @param df A data frame containing analysis results to be plotted
#' @param tie_handling A string specifying ....
#'
#'
#'@details
#'Additional details...
#'
#'
#'@return
#' An object of type \code{patchwork} containing plot showing blah de blah
#'
#'
#'
#'
#' @export
plot.WinRatio <- function(wr)
{
        # Extract data and create dataframe
        wr_output_list <- lapply(1:length(wr$call$outcomes), function(i) {

                # Get estimates for win ratio based on outcome i and outcomes 1 to i
                tmp_wr_individual <- WinRatio::winratio(id = wr$call$id, trt = wr$call$trt, active = wr$call$active,
                                              outcomes = wr$call$outcomes[i],
                                              fu = wr$call$fu, data = wr$call$data)

                tmp_wr_cumulative <- WinRatio::winratio(id = wr$call$id, trt = wr$call$trt, active = wr$call$active,
                                outcomes = wr$call$outcomes[1:i],
                                fu = wr$call$fu, data = wr$call$data)

                data.frame(level=i, level_names=names(tmp_wr_cumulative$call$outcome[i]),
                           win_inherit=(tmp_wr_cumulative$total.wins-tmp_wr_cumulative$wins[i]), win=tmp_wr_cumulative$wins[i],
                           tie=tmp_wr_cumulative$total.ties,
                           loss=tmp_wr_cumulative$loss[i], loss_inherit=(tmp_wr_cumulative$total.loss-tmp_wr_cumulative$loss[i]),
                           win_ratio_individual=tmp_wr_individual$wr,
                           ci_lower_individual=tmp_wr_individual$wr.lower, ci_upper_individual=tmp_wr_individual$wr.upper,
                           win_ratio_cumulative=tmp_wr_cumulative$wr,
                           ci_lower_cumulative=tmp_wr_cumulative$wr.lower, ci_upper_cumulative=tmp_wr_cumulative$wr.upper
                           )
        }
        )

        wr_dataframe <- do.call("rbind",wr_output_list)

        # Create a dataframe with overall data line info
        overall_line <- data.frame(level=(length(wr$call$outcomes)+1), level_names="Overall", win_inherit=0, win=wr$total.wins,
                                   tie=wr$total.ties, loss=wr$total.loss, loss_inherit=0,
                                   win_ratio_individual=wr$wr, ci_lower_individual=wr$wr.lower, ci_upper_individual=wr$wr.upper,
                                   win_ratio_cumulative=wr$wr, ci_lower_cumulative=wr$wr.lower, ci_upper_cumulative=wr$wr.upper)

        # Bind overall line dataframe to wr_dataframe for plotting
        df <- rbind(wr_dataframe, overall_line)

        # Plot figure
        WinRatioPlot::winRatioPlot(df, tie_handling="drop", type="wr")

}


