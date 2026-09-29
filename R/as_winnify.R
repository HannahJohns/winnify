#' Convert other objects to a winnify object
#' @aliases as_winnify
#'
#' @usage as_winnify(x, ...)
#' @param x an object used to select a method
#' @param ... further arguments passed to other methods
#'
#' @details as_winnify methods exist for the following objects:
#' \itemize{
#' \item data.frame objects from base R. See \code{\link{as_winnify.data.frame}} for details.
#' \item WinRatio objects returned by the \code{\link[WinRatio]{winratio}} function. See \code{\link{as_winnify.WinRatio}} for details.
#' }
"as_winnify"

#' Convert a data frame to a Winnify object
#'
#' @exportS3Method
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
as_winnify.data.frame <- function(x,
                                  effect_size = NULL,
                                  neutral_point=NULL,
                                  tie_handling=NULL,
                                  outcome_names = "outcome_names",
                                  win_cumulative="win_cumulative",
                                  tie_cumulative="tie_cumulative",
                                  loss_cumulative="loss_cumulative",
                                  estimate_cumulative ="estimate_cumulative",
                                  lower_cumulative="lower_cumulative",
                                  upper_cumulative ="upper_cumulative",
                                  pval_cumulative ="pval_cumulative",
                                  estimate_individual ="estimate_individual",
                                  lower_individual ="lower_individual",
                                  upper_individual ="upper_individual",
                                  pval_individual ="pval_individual",
                                  ignore_missing = FALSE,
                                  ...
                                  )
{

        # list columns cause things to break - fix this here.
        for(i in 1:ncol(x)) x[,i] <- unlist(x[,i])

        target_variables <- list(
                effect_size = effect_size,
                neutral_point =neutral_point,
                tie_handling= tie_handling,
                outcome_names=outcome_names,
                win_cumulative=win_cumulative,
                tie_cumulative=tie_cumulative,
                loss_cumulative=loss_cumulative,

                estimate_cumulative=estimate_cumulative,
                lower_cumulative=lower_cumulative,
                upper_cumulative=upper_cumulative,
                pval_cumulative=pval_cumulative,

                estimate_individual=estimate_individual,
                lower_individual=lower_individual,
                upper_individual=upper_individual,
                pval_individual=pval_individual
        )



        for(i in 1:length(target_variables)){

                this_target  <- names(target_variables)[i]
                this_location <- target_variables[[i]]

                if(this_target == "effect_size"){
                        # NOTE: This will cause problems if effect size is never
                        # not the first item in the list.
                        if(is.null(this_location)){
                                if("effect_size" %in% colnames(x)){
                                        out <- x[,"effect_size",drop=FALSE]
                                } else {
                                        out <- data.frame(effect_size = rep("Effect Size",nrow(x)))
                                }
                        } else {
                                if(this_location %in% colnames(x)){
                                        out <- x[,this_location,drop=FALSE]
                                } else {
                                        out <- data.frame(effect_size = rep(this_location,nrow(x)))
                                }
                        }
                } else if(this_target == "neutral_point"){
                        if(is.null(this_location)){
                                if("neutral_point" %in% colnames(x)){
                                        out <- cbind(out,neutral_point = x[,"neutral_point"])
                                } else {
                                        # Do nothing - plot.Winninfy() looks for null values to know not to
                                        # print the direction of effect
                                }
                        } else {
                                if(this_location %in% colnames(x)){
                                        out <- cbind(out,neutral_point = x[,this_location])
                                } else {
                                        out <- cbind(out,data.frame(neutral_point = rep(this_location,nrow(x))))
                                }
                        }
                } else if(this_target == "tie_handling"){
                        if(is.null(this_location)){
                                if("tie_handling" %in% colnames(x)){
                                        out <- cbind(out,tie_handling = x[,"tie_handling"])
                                } else {
                                        # Do nothing - plot.Winninfy() looks for null values to know not to
                                        # print the tie splitting line
                                }
                        } else {
                                if(this_location %in% colnames(x)){
                                        out <- cbind(out,tie_handling = x[,this_location])
                                } else {
                                        out <- cbind(out,data.frame(tie_handling = rep(this_location,nrow(x))))
                                }
                        }
                } else {
                        if(!is.null(this_location)){

                                if(this_location %in% colnames(x)){
                                        out <- cbind(out,x[,this_location])
                                        colnames(out)[ncol(out)] <- this_target
                                } else {
                                        if(ignore_missing){
                                                warning(sprintf("Missing variable %s",this_location))
                                                out <- cbind(out,rep(NA,nrow(x)))
                                                colnames(out)[ncol(out)] <- this_target
                                        } else {
                                                stop(sprintf("Missing variable %s",this_location))
                                        }

                                }
                        } else {
                                out <- cbind(out,rep(NA,nrow(x)))
                                colnames(out)[ncol(out)] <- this_target
                        }
                }
        }


        out <- by(out,out$effect_size, function(this_out){

                this_out <<- this_out

                this_out <- this_out[order(this_out$outcome_names),]

                if(nrow(this_out)>1){

                        this_out$win_inherit <- 0
                        this_out$loss_inherit <- 0

                        this_out$win_inherit[-1] <- this_out$win_cumulative[-nrow(this_out)]
                        this_out$loss_inherit[-1] <- this_out$loss_cumulative[-nrow(this_out)]

                        this_out$win <- this_out$win_cumulative - this_out$win_inherit
                        this_out$loss <- this_out$loss_cumulative - this_out$loss_inherit
                }

                if(any(this_out$win < 0) | any(this_out$loss < 0) ){
                        this_out$win <- this_out$win_cumulative
                        this_out$loss <- this_out$loss_cumulative
                        this_out$win_inherit <- NA
                        this_out$loss_inherit <- NA
                        warning("wins and losses do not increase with successive levels. Incremental wins/losses vs. inherited wins/losses could not be calculated")
                }


                overall_row <- this_out[nrow(this_out),,drop=FALSE]

                overall_row$outcome_names <- "Overall"
                overall_row[,c("estimate_individual","lower_individual","upper_individual","pval_individual")] <- overall_row[,c("estimate_cumulative","lower_cumulative","upper_cumulative","pval_cumulative")]
                overall_row$win <- overall_row$win+overall_row$win_inherit
                overall_row$loss <- overall_row$loss+overall_row$loss_inherit


                if(any(is.na(overall_row$win))){
                        overall_row$loss <- overall_row$loss_cumulative
                        overall_row$win <- overall_row$win_cumulative
                } else {
                        overall_row$loss_inherit <- overall_row$win_inherit <- 0
                }

                return(rbind(this_out,overall_row))
        })

        out <- do.call("rbind",out)
        rownames(out) <- NULL
        class(out) <- c("Winnify",class(out))

        out
}




#' Visualise Win Ratio effect estimates from the winratio function
#'
#' Convenience function for visualising Win Ratio as estimated by \code{\link[WinRatio]{winratio}}
#' from the \code{WinRatio} package.
#'
#' @exportS3Method
#'
#' @usage
#' as_winnify(x)
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
as_winnify.WinRatio <- function(x,...){

        stop("this needs reworked")

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









