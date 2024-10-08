#' Visualise Win Ratio effect estimates from the winratio function
#'
#' Extracts necessary data to visualise the Win Ratio from output of
#' the winratio function in the WinRatio package (CITE).
#'
#' @usage
#' plot.WinRatio(wr)
#'
#' @param wr An object of class \code{WinRatio} containing the output from the winratio
#' function in the WinRatio package.
#'
#'
#' @details
#' The win ratio plot dataframe function is a way to visualise win ratio estimates
#' calculated directly from the winratio function from the WinRatio package. This
#' is done with two plots:
#' \describe{
#' \item{}{The left plot visualises the contribution of each outcome level
#' of the prioritised outcome hierarchy to the overall proportion of wins, losses
#' and ties. At each successive outcome level, ties remaining from the previous level are
#' resolved and then passed onto the next outcome.}
#' \item{}{The right plot presents a forest plot of the individual outcome effect
#' estimate as well as the cumulative effect estimate to show how the effect
#' estimate shifts as successive outcomes are considered.}
#' }
#'
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
#' @examples
#' # Use the test_data dataset provided by the package and visualise the prioritised outcome effect estimate.
#' # test_data dataset taken from ... (CITE!!!)
#' data_test <- read.csv("test_data.csv")
#' wr <- WinRatio::winratio(id = "id", trt = "trt", active = "Treatment",
#'                         outcomes = list("Death" = c("death", "s", "t2death"),
#'                         "Length of hospital stay" = list(paste0("recurr", 1:9), "r", paste0("t2recurr", 1:9)),
#'                         "Wellbeing at 28 days" = c("size", "c", ">")),
#'                         fu = "t2death", data = data_test)
#' WinRatioPlot::plot.WinRatio(wr)
#'
#'
#' @references
#' ...ADD CITATION FOR R PACKAGE!!
#'
#'
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


