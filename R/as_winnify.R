#' Convert a data frame to a Winnify object
#'
#' @usage
#' as_winnify.data.frame(data,
#' outcome_names = "outcome_names",
#' win_inherit="win_inherit",
#' win="win",
#' tie="tie",
#' loss="loss",
#' loss_inherit="loss_inherit",
#' estimate_cumulative ="estimate_cumulative",
#' lower_cumulative="lower_cumulative",
#' upper_cumulative ="upper_cumulative ",
#' pval_cumulative ="pval_cumulative",
#' estimate_individual ="estimate_individual",
#' lower_individual ="lower_individual",
#' upper_individual ="upper_individual",
#' pval_individual ="pval_individual",
#' neutral_point=NULL, tie_handling=NULL, effect_name=NULL,
#' ignore_missing = FALSE
#' )
#'
#' @param data A data frame containing analysis results
#' @param outcome_names A string indicating the column of \code{data} containing the names
#' of the outcomes.
#' @param win_inherit A string indicating the column of \code{data} containing the
#' inherited wins for each outcome.
#' @param win A string indicating the column of \code{data} containing the wins for
#' each outcome.
#' @param tie A string indicating the column of \code{data} containing the ties for
#' each outcome.
#' @param loss A string indicating the column of \code{data} containing the losses for
#' each outcome.
#' @param loss_inherit A string indicating the column of \code{data} containing the
#' inherited losses for each outcome.
#'
#' @param estimate_cumulative A string indicating the column of \code{data} containing
#' the cumulative effect size for each outcome when considered ranked.
#' @param lower_individual A string indicating the column of \code{data} containing
#' the lower boundary of the confidence interval for the cumulative effect size for
#' each outcome when considered ranked.
#' @param upper_individual A string indicating the column of \code{data} containing
#' the upper boundary of the confidence interval for the cumulative effect size for
#' each outcome when considered ranked.
#' @param pval_cumulative A string indicating the column of \code{data} containing
#' the p value for estimated cumulative effect size for
#' each outcome when considered ranked.
#'
#' @param estimate_individual A string indicating the column of \code{data} containing
#' the effect size for each outcome when considered independently (not ranked).
#' @param ci_lower_individual A string indicating the column of \code{data} containing
#' the lower boundary of the confidence interval for the effect size for each outcome
#' when considered independently (not ranked).
#' @param ci_upper_individual A string indicating the column of \code{data} containing
#' the upper boundary of the confidence interval for the effect size for each outcome
#' when considered independently (not ranked).
#' @param pval_individual A string indicating the column of \code{data} containing
#' the p value for estimated cumulative effect size for
#' each outcome when considered independently (not ranked).
#'
#' @param effect_name A string specifying type of analysis to appear in the forest plot label.
#' @param neutral_point A number specifying the effect size that corresponds to no effect.
#' @param tie_handling A string specifying how ties should be treated.
#' @param ignore_missing A logical indicating if missing columns are allowed. If \code{TRUE}, missing columns are replaced with \code{NA}
#'
#' @examples
#'
#'

#' data <- data.frame(outcome_names=c("Death", "Length of hospital stay", "Time off work", "Wellbeing"),
#'                    win_inherit=c(0,15,25,33),
#'                    win=c(15,10,8,15),
#'                    tie=c(80,60,40,15),
#'                    loss=c(5,10,12,10),
#'                    loss_inherit=c(0,5,15,27),
#'                    win_ratio=c(3,1,0.67,1.5),
#'                    ci_lower=c(2.6,0.6,0.27,1.1),
#'                    ci_upper=c(3.4,1.4,1.07,1.9),
#'                    win_ratio_cum=c(3,1.67,1.22,1.30),
#'                    ci_lower_cum=c(2.6,1.32,0.92,1.10),
#'                    ci_upper_cum=c(3.4,2.02,1.52,1.50),
#'                    pval_cum=NA,
#'                    pval=NA
#' )
#'
#'
#' result <- as_winnify(data,
#'                      estimate_cumulative = "win_ratio_cum",
#'                      lower_cumulative="ci_lower_cum",
#'                      upper_cumulative ="ci_upper_cum",
#'                      estimate_individual = "win_ratio",
#'                      lower_individual="ci_lower",
#'                      upper_individual ="ci_upper",
#'                      pval_cumulative = "pval_cum",
#'                      pval_individual = "pval",
#'                      neutral_point = 1
#' )
#' plot(result)
#'
#'
as_winnify.data.frame <- function(data,
                                  outcome_names = "outcome_names",
                                  win_inherit="win_inherit",
                                  win="win",
                                  tie="tie",
                                  loss="loss",
                                  loss_inherit="loss_inherit",
                                  estimate_cumulative ="estimate_cumulative",
                                  lower_cumulative="lower_cumulative",
                                  upper_cumulative ="upper_cumulative ",
                                  pval_cumulative ="pval_cumulative",
                                  estimate_individual ="estimate_individual",
                                  lower_individual ="lower_individual",
                                  upper_individual ="upper_individual",
                                  pval_individual ="pval_individual",
                                  neutral_point=NULL, tie_handling=NULL, effect_name=NULL,
                                  ignore_missing = FALSE
                                  )
{

        # Include some checks to ensure that the argument inserted is a dataframe and that the necessary variables
        # are included in the dataframe and stop and report an error message if they are missing


        target_names <- c(outcome_names="outcome_names",

                          win_inherit="win_inherit",
                          win="win",
                          tie="tie",
                          loss="loss",
                          loss_inherit="loss_inherit",

                          estimate_cumulative="estimate_cumulative",
                          lower_cumulative="lower_cumulative",
                          upper_cumulative="upper_cumulative",
                          pval_cumulative ="pval_cumulative",

                          estimate_individual="estimate_individual",
                          lower_individual="lower_individual",
                          upper_individual="upper_individual",
                          pval_individual="pval_individual"
                          )

        names(target_names) <- c(outcome_names,
                                 win_inherit, win, tie, loss, loss_inherit,
                                 estimate_cumulative, lower_cumulative, upper_cumulative,pval_cumulative,
                                 estimate_individual, lower_individual, upper_individual,pval_individual
                                 )

        if(!all(names(target_names) %in% names(data))) {

                unfound_values <- setdiff(names(target_names), colnames(data))

                if(ignore_missing){

                        for(i in unfound_values) data[,i] <- NA

                        unfound_values <- paste(unfound_values,collapse=", ")
                        warning(sprintf("Missing variables for: %s. Check input dataframe contains these variables.",unfound_values))


                } else {

                        unfound_values <- paste(unfound_values,collapse=", ")
                        stop(sprintf("Missing variables for: %s. Check input dataframe contains these variables.",unfound_values))
                }


        }


        # Select only necessary columns (in case extra columns in data) and reorder columns so that they are in correct order for the renaming function
        out <- data[,c(outcome_names,
                    win_inherit, win, tie, loss, loss_inherit,
                    estimate_cumulative, lower_cumulative, upper_cumulative,pval_cumulative,
                    estimate_individual, lower_individual, upper_individual,pval_individual
                    )
                 ]

        # Rename the columns to the necessary names for Winnify objects
        colnames(out) <- target_names


        overall_row <- out[nrow(out),,drop=FALSE]

        overall_row$outcome_names <- "Overall"
        overall_row[,c("estimate_individual","lower_individual","upper_individual","pval_individual")] <- overall_row[,c("estimate_cumulative","lower_cumulative","upper_cumulative","pval_cumulative")]
        overall_row$win <- overall_row$win+overall_row$win_inherit
        overall_row$loss <- overall_row$loss+overall_row$loss_inherit
        overall_row$loss_inherit <- overall_row$win_inherit <- 0

        out <- rbind(out,overall_row)

        attr(out,"tie_handling") <- tie_handling
        attr(out,"neutral_point") <- neutral_point
        attr(out,"effect_name") <- effect_name

        class(out) <- c("Winnify",class(out))

        out
}



#' Visualise Win Ratio effect estimates from the winratio function
#'
#' Convenience function for visualising Win Ratio as estimated by \code{\link[WinRatio]{winratio}}
#' from the \code{WinRatio} package.
#'
#' @usage
#' as_winnify.winratio(x)
#'
#' @param x An object of class \code{WinRatio} containing the output from \code{winratio()}
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
#' library(WinRatio)
#'
#' # winratio function requires additional inputs
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
#' result <- as_winnify(wr)
#' plot(result)
#'
as_winnify.WinRatio <- function(x){
        # Extract data and create dataframe
        wr_output_list <- lapply(1:length(x$call$outcomes), function(i) {

                # Get estimates for win ratio based on outcome i and outcomes 1 to i
                tmp_wr_individual <- WinRatio::winratio(id = x$call$id, trt = x$call$trt, active = x$call$active,
                                                        outcomes = x$call$outcomes[i],
                                                        fu = x$call$fu, data = x$call$data)

                tmp_wr_cumulative <- WinRatio::winratio(id = x$call$id, trt = x$call$trt, active = x$call$active,
                                                        outcomes = x$call$outcomes[1:i],
                                                        fu = x$call$fu, data = x$call$data)

                data.frame(outcome_names=names(tmp_wr_cumulative$call$outcome[i]),
                           win_inherit=(tmp_wr_cumulative$total.wins-tmp_wr_cumulative$wins[i]), win=tmp_wr_cumulative$wins[i],
                           tie=tmp_wr_cumulative$total.ties,
                           loss=tmp_wr_cumulative$loss[i], loss_inherit=(tmp_wr_cumulative$total.loss-tmp_wr_cumulative$loss[i]),
                           estimate_cumulative =tmp_wr_cumulative$wr,
                           lower_cumulative=tmp_wr_cumulative$wr.lower, upper_cumulative =tmp_wr_cumulative$wr.upper,
                           pval_cumulative =tmp_wr_cumulative$p.value,
                           estimate_individual =tmp_wr_individual$wr,
                           lower_individual =tmp_wr_individual$wr.lower, upper_individual =tmp_wr_individual$wr.upper,
                           pval_individual=tmp_wr_individual$p.value
                )
        }
        )

        out <- do.call("rbind",wr_output_list)

        overall_row <- out[nrow(out),,drop=FALSE]

        overall_row$outcome_names <- "Overall"
        overall_row[,c("estimate_individual","lower_individual","upper_individual","pval_individual")] <- overall_row[,c("estimate_cumulative","lower_cumulative","upper_cumulative","pval_cumulative")]
        overall_row$win <- overall_row$win+overall_row$win_inherit
        overall_row$loss <- overall_row$loss+overall_row$loss_inherit
        overall_row$loss_inherit <- overall_row$win_inherit <- 0

        out <- rbind(out,overall_row)

        attr(out,"tie_handling") <- "drop"
        attr(out,"neutral_point") <- 1
        attr(out,"effect_name") <- "Win Ratio"

        class(out) <- c("Winnify",class(out))

        out

}









