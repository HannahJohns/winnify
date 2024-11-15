#' Visualise Win Ratio effect estimates from the winratio function
#'
#' Convenience function for visualising Win Ratio as estimated by \code{\link[WinRatio]{winratio}}
#' from the \code{WinRatio} package.
#'
#' @usage
#' plot.winratio(wr)
#'
#' @param wr An object of class \code{WinRatio} containing the output from \code{winratio()}
#'
#' @return
#' A \code{patchwork} object containing two plots (left and right):
#' \describe{
#' \item{}{The left plot shows the proportions of inherited
#' wins/wins/ties/losses/inherited losses for each outcome when ranked and overall.}
#' \item{}{The right plot shows the measured effect (e.g., win ratio) at both a
#' cumulatively ranked outcome level (with overall) and for each outcome independently.}
#' }
#'
#' @seealso \link{po_plot}
#'
#'
#' @examples
#' # Load WinRatio package to use winratio function
#' library(WinRatio)
#'
#' # Use remapcap dataset provided by package and modify so it is suitable to use with
#' # the winratio function
#'
#' # Add an id variable to dataset
#' remapcap$id <- seq_len(nrow(remapcap))
#'
#' # Calculate time at home as time post discharge
#' remapcap$homeTime <- (remapcap$deathTime-remapcap$dischargeTime)
#'
#' # Collapse treatment groups so only one treatment group
#' remapcap$group <- ifelse(remapcap$group == "uc", "Control", "Treatment")
#'
#' # Run winratio function on dataset
#' wr <- winratio(id = "id", trt = "group", active = "Treatment",
#'                 outcomes = list("Death" = c("deathStatus", "s", "deathTime"),
#'                 "Time at home" = c("homeTime", "c", "<")),
#'                 fu = "deathTime", data = remapcap)
#'
#' # Plot win ratio results
#' plot.winratio(wr)
#'
#'
#'
plot.winratio <- function(wr)
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

                data.frame(outcome_names=names(tmp_wr_cumulative$call$outcome[i]),
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

        df <- do.call("rbind",wr_output_list)


        # Plot figure
        WinRatioPlot::winRatioPlot(df, effect_name = "Win ratio", neutral_point = 1, tie_handling="drop")

}


