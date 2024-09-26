#' Visualise prioritised outcome effect estimates
#'
#' Visualises the contribution of individual outcomes under generalized pairwise comparisons with prioritized
#' outcomes methods, e.g. Win Ratio (CITE), Win Odds (CITE) and Net Benefit (Buyse 2010).
#'
#'
#' @usage
#' winRatioPlot(df, effect_name, neutral_point=NULL, tie_handling=NULL,
#' overall_line=TRUE, level="level", level_names="level_names",
#' win_inherit="win_inherit", win="win", tie="tie", loss="loss",
#' loss_inherit="loss_inherit", win_ratio_individual="win_ratio_individual",
#' ci_lower_individual="ci_lower_individual", ci_upper_individual="ci_upper_individual",
#' win_ratio_cumulative="win_ratio_cumulative", ci_lower_cumulative="ci_lower_cumulative",
#' ci_upper_cumulative="ci_upper_cumulative")
#'
#' @param df A data frame containing the analysis results to be plotted.
#' @param effect_name A string specifying type of analysis to appear in the forest plot name.
#' @param neutral_point An integer specifying the point of no effect for intervention vs control.
#' @param tie_handling A string specifying how ties should be treated.
#' Should be set to \code{"drop"} or \code{"split"}.
#' @param overall_line If \code{TRUE} then the \code{df} already contains data for an overall line,
#' if \code{FALSE} the \code{df} does not contain this data and an overall line for the plot
#' will be calculated. Default is \code{TRUE}.
#' @param level A string indicating the column of \code{df} containing the ranking of
#' the outcomes.
#' @param level_names A string indicating the column of \code{df} containing the names
#' of the outcomes.
#' @param win_inherit A string indicating the column of \code{df} containing the
#' inherited wins for each outcome.
#' @param win A string indicating the column of \code{df} containing the wins for
#' each outcome.
#' @param tie A string indicating the column of \code{df} containing the ties for
#' each outcome.
#' @param loss A string indicating the column of \code{df} containing the losses for
#' each outcome.
#' @param loss_inherit A string indicating the column of \code{df} containing the
#' inherited losses for each outcome.
#' @param win_ratio_individual A string indicating the column of \code{df} containing
#' the win ratio for each outcome when considered independently (not ranked).
#' @param ci_lower_individual A string indicating the column of \code{df} containing
#' the lower boundary of the confidence interval for the win ratio for each outcome
#' when considered independently (not ranked).
#' @param ci_upper_individual A string indicating the column of \code{df} containing
#' the upper boundary of the confidence interval for the win ratio for each outcome
#' when considered independently (not ranked).
#' @param win_ratio_cumulative A string indicating the column of \code{df} containing
#' the win ratio for each outcome when considered ranked.
#' @param ci_lower_cumulative A string indicating the column of \code{df} containing
#' the lower boundary of the confidence interval for the win ratio for each outcome
#' when considered ranked.
#' @param ci_upper_cumulative A string indicating the column of \code{df} containing
#' the upper boundary of the confidence interval for the win ratio for each outcome
#' when considered ranked.
#'
#'
#' @details
#' The win ratio plot function is a way to visualise win ratio (or win odds) data,
#' which incorporates ranking outcomes in order of importance and then assessing
#' pairwise comparisons of participants for wins/losses/ties for the intervention vs
#' control treatments. A "win" is recorded if the intervention participant did better
#' on the outcome than the pairwise control participant, a "tie" is recorded if the
#' intervention participant did the same on the outcome than the pairwise control
#' participant, and a "loss" is recorded if the intervention participant did worse on
#' the outcome than the pairwise control participant. The win ratio plot presents this
#' data visually with two plots. The left plot shows the proportions of
#' inherited wins/wins/ties/losses/inherited losses for each outcome as well as an overall
#' line with the overall wins/ties/losses once all outcomes have been ranked. The right
#' plot presents the individual outcome effect estimate independently as well as the
#' cumulative effect estimate as a forest plot to clearly indicate how the effect
#' estimate shifts as each additional ranked outcome is considered.
#'
#' The \code{df} must contain variables for all the arguments containing necessary data
#' to produce the plots. This includes parameters: \code{level_names, win_inherit, win,
#' tie, loss, loss_inherit, win_ratio_individual, ci_lower_individual,
#' ci_upper_individual, win_ratio_cumulative, ci_lower_cumulative, ci_upper_cumulative}.
#' The \code{level} parameter is optional to specify the ranking of outcomes, if included,
#' the data within the variable should be integers and the order should be from "1" for
#' the first level outcome, "2" for the second level outcome etc. (if included, the
#' overall row can just be numbered as the number of ranked outcomes plus one). Variables
#' for parameters \code{effect_name, neutral_point, tie_handling and overall_line} do not
#' need to be included in the \code{df}.
#'
#' \code{effect_name} specifies the type of analysis the data is for. The string specified
#' will be used as the axis label for the forest plot and in the legend. For example,
#' \code{"Win ratio"} could be provided for a win ratio analysis or \code{"Adjusted win ratio"}
#' for an adjusted win ratio analysis. This argument must be specified.
#'
#' \code{neutral_point} specifies the point of no effect for intervention vs control. This
#' should be a number. For example, for a ratio this would be 1.
#'
#' \code{tie_handling} specifies how ties are handled in the original analysis so that a
#' dashed line at the position ties were assigned to the wins and losses in the overall
#' row can be added. This should be set to \code{"drop"} or \code{"split"}. If not
#' specified no dashed line will appear indicating the position of splitting of ties
#' for the overall level.
#'
#' \code{overall_line==TRUE} indicates that the \code{df} already contains data for an
#' overall line. \code{overall_line==FALSE} indicates that the \code{df} does not
#' contain this data and an overall line for the plot will be calculated from the data
#' in the bottom ranked outcome row. The default for the parameter is \code{TRUE}.
#'
#' \code{level} specifies the column of the \code{df} containing the ranking of outcomes.
#' The data within the variable should be integers and the order should be from "1" for
#' the first level outcome, "2" for the second level outcome etc. (if included, the
#' overall row can just be numbered as the number of ranked outcomes plus one). Note,
#' \code{level} is optional, but if not included in the \code{df} the ranking of
#' outcomes will be automatically inferred as the row order within the \code{df}. The
#' default name for this column in the \code{df} is \code{"level"}.
#'
#' \code{level_names} specifies the column of the \code{df} containing the names of
#' the outcomes. The data within the variable should be strings, these strings will
#' appear as the names of the outcomes in the plot. For example, an outcome name within
#' this column of the \code{df} might be \code{"Length of stay in hospital"}. Note, the
#' default name for this column in the \code{df} is \code{"level_names"}.
#'
#' \code{win_inherit} specifies the column of the \code{df} containing the inherited wins
#' for each outcome when ranked. The data within the variable should be integers.
#' Inherited wins are defined as the total number of wins from any previous levels of
#' outcomes, and the first level outcome should have a value of \code{0} as there cannot
#' be any inherited wins for the first level outcome. The final \code{"overall"} row
#' value should also be set to \code{0}, if it is not set to \code{0} then the overall
#' row will incorrectly show proportions for inherited wins as well instead of just
#' wins/ties/losses. Note, the default name for this column in the \code{df} is
#' \code{"win_inherit"}.
#'
#' \code{win} specifies the column of the \code{df} containing the wins for the
#' intervention treatment for each outcome when ranked. The data within the variable
#' should be integers. Wins are defined as the number of pairwise comparisons of
#' intervention vs control treatment where the participant receiving the intervention
#' treatment had a better result on the outcome compared to the pairwise comparative
#' participant receiving the control treatment for that outcome. For each level of
#' outcome down the ranking, wins can only be calculated from pairwise comparisons of
#' participants that tied on the outcome level above. The final \code{"overall"} row value should
#' be set to the sum of \code{win_inherit} and \code{win} for the final level outcome,
#' if it is not set to this the overall row will show incorrect overall proportions
#' for wins/ties/losses. Note, the default name for this column in the \code{df} is
#' \code{"win"}.
#'
#' \code{tie} specifies the column of the \code{df} containing the ties for the
#' intervention and control treatments for each outcome when ranked. The data within
#' the variable should be integers. Ties are defined as the number of pairwise
#' comparisons of intervention vs control treatment where the participant receiving
#' the intervention treatment had the same result on the outcome compared to the pairwise comparative
#' participant receiving the control treatment for that outcome. For each level of
#' outcome down the ranking, ties can only be calculated from pairwise comparisons of
#' participants that tied on the level above. The final \code{"overall"} row value should
#' be set to the the \code{tie} value for the final level outcome, if it is not set to
#' this the overall row will show incorrect overall proportions for wins/ties/losses.
#' Note, the default name for this column in the \code{df} is \code{"tie"}.
#'
#' \code{loss} specifies the column of the \code{df} containing the losses for the
#' intervention treatment for each outcome when ranked. The data within the variable
#' should be integers. Losses are defined as the number of pairwise comparisons of
#' intervention vs control treatment where the participant receiving the intervention
#' treatment had a worse result on the outcome compared to the pairwise comparative
#' participant receiving the control treatment for that outcome. For each level of
#' outcome down the ranking, losses can only be calculated from pairwise comparisons of
#' participants that tied on the outcome level above. The final \code{"overall"} row value should
#' be set to the sum of \code{loss_inherit} and \code{loss} for the final level outcome,
#' if it is not set to this the overall row will show incorrect overall proportions
#' for wins/ties/losses. Note, the default name for this column in the \code{df} is
#' \code{"loss"}.
#'
#' \code{loss_inherit} specifies the column of the \code{df} containing the inherited losses
#' for each outcome when ranked. The data within the variable should be integers.
#' Inherited losses are defined as the total number of losses from any previous levels of
#' outcomes, and the first level outcome should have a value of \code{0} as there cannot
#' be any inherited losses for the first level outcome. The final \code{"overall"} row
#' value should also be set to \code{0}, if it is not set to \code{0} then the overall
#' row will incorrectly show proportions for inherited losses as well instead of just
#' wins/ties/losses. Note, the default name for this column in the \code{df} is
#' \code{"loss_inherit"}.
#'
#' \code{win_ratio_individual} specifies the column of the \code{df} containing the win
#' ratio (or other alternative effect measure, such as odds ratio) for each outcome when
#' considered independently (not ranked). This is defined as the win ratio for each
#' outcome if it was the only outcome considered, i.e., the ratio of wins to losses for
#' each pairwise comparison of intervention vs control treatment without considering any
#' other higher ranked outcomes. The data within the variable should be numeric. The final
#' \code{"overall"} row value can be set to the same value for the
#' \code{win_ratio_individual} as the final level outcome as the \code{"overall"} row
#' for this variable is not used in the production of the plot. Note, the default name
#' for this column in the \code{df} is \code{"win_ratio_individual"}.
#'
#' \code{ci_lower_individual} specifies the column of the \code{df} containing the lower
#' boundary of the confidence interval for the \code{win_ratio_individual} (the win
#' ratio when considered independently, not ranked). The data within the variable should
#' be numeric. The final \code{"overall"} row value can be set to the same value for the
#' \code{ci_lower_individual} as the final level outcome as the \code{"overall"} row
#' for this variable is not used in the production of the plot. Note, the default name
#' for this column in the \code{df} is \code{"ci_lower_individual"}.
#'
#' \code{ci_upper_individual} specifies the column of the \code{df} containing the upper
#' boundary of the confidence interval for the \code{win_ratio_individual} (the win
#' ratio when considered independently, not ranked). The data within the variable should
#' be numeric. The final \code{"overall"} row value can be set to the same value for the
#' \code{ci_upper_individual} as the final level outcome as the \code{"overall"} row
#' for this variable is not used in the production of the plot. Note, the default name
#' for this column in the \code{df} is \code{"ci_upper_individual"}.
#'
#' \code{win_ratio_cumulative} specifies the column of the \code{df} containing the win
#' ratio (or other alternative effect measure, such as odds ratio) for each outcome when
#' considered ranked. This is defined as the ratio of the sum of \code{win_inherit} and
#' \code{win} for that outcome divided by the sum of \code{loss_inherit} and \code{loss}
#' for that outcome. The data within the variable should be numeric. The final
#' \code{"overall"} row value can be set to the same value for the
#' \code{win_ratio_cumulative} as the final level outcome as the \code{"overall"} row
#' for this variable is the same value as the final level outcome of the
#' \code{win_ratio_cumulative}. Note, the default name for this column in the \code{df}
#' is \code{"win_ratio_cumulative"}.
#'
#' \code{ci_lower_cumulative} specifies the column of the \code{df} containing the lower
#' boundary of the confidence interval for the \code{win_ratio_cumulative} (the win
#' ratio when considered ranked). The data within the variable should be numeric. The
#' final \code{"overall"} row value can be set to the same value for the
#' \code{ci_lower_cumulative} as the final level outcome as the \code{"overall"} row
#' for this variable is the same value as the final level outcome of the
#' \code{ci_lower_cumulative}. Note, the default name for this column in the \code{df}
#' is \code{"ci_lower_cumulative"}.
#'
#' \code{ci_upper_cumulative} specifies the column of the \code{df} containing the upper
#' boundary of the confidence interval for the \code{win_ratio_cumulative} (the win
#' ratio when considered ranked). The data within the variable should be numeric. The
#' final \code{"overall"} row value can be set to the same value for the
#' \code{ci_upper_cumulative} as the final level outcome as the \code{"overall"} row
#' for this variable is the same value as the final level outcome of the
#' \code{ci_upper_cumulative}. Note, the default name for this column in the \code{df}
#' is \code{"ci_upper_cumulative"}.
#'
#'
#' @return
#' A \code{patchwork} object containing two plots (left and right):
#' \describe{
#'      \item{The left plot shows the proportions of inherited
#'      wins/wins/ties/losses/inherited losses for each outcome when ranked and overall.
#'      The right plot shows the measured effect (e.g., win ratio) at both a
#'      ranked outcome level (with overall) and for each outcome independently.}
#' }
#'
#'
#'
#'
#' @references
#' Buyse M. Generalized pairwise comparisons of prioritized outcomes in the two-sample problem. Stat Med. 2010 Dec 30;29(30):3245-57. doi: 10.1002/sim.3923. PMID: 21170918.
#'
#'
winRatioPlot <- function(df, effect_name, neutral_point=NULL, tie_handling=NULL, overall_line=TRUE,
                         level="level", level_names="level_names", win_inherit="win_inherit", win="win", tie="tie",
                         loss="loss", loss_inherit="loss_inherit",
                         win_ratio_individual="win_ratio_individual", ci_lower_individual="ci_lower_individual", ci_upper_individual="ci_upper_individual",
                         win_ratio_cumulative="win_ratio_cumulative", ci_lower_cumulative="ci_lower_cumulative", ci_upper_cumulative="ci_upper_cumulative")
{
        # If level ranking column "level" does not exist in df then first create this
        if((level == "level") & ("level" %in% colnames(df) == FALSE)) {
                warning("level column not found. It has been added automatically")
                df <- cbind(level = 1:nrow(df), df)
        }

        # Include some checks to ensure that the argument inserted is a dataframe and that the necessary variables
        # are included in the dataframe and stop and report an error message if they are missing
        target_names <- c(level="level", level_names="level_names", win_inherit="win_inherit", win="win", tie="tie",
                          loss="loss", loss_inherit="loss_inherit",
                          win_ratio="win_ratio_individual", ci_lower="ci_lower_individual", ci_upper="ci_upper_individual",
                          win_ratio_cum="win_ratio_cumulative", ci_lower_cum="ci_lower_cumulative", ci_upper_cum="ci_upper_cumulative")

        names(target_names) <- c(level, level_names, win_inherit, win, tie, loss, loss_inherit, win_ratio_individual,
                                 ci_lower_individual, ci_upper_individual, win_ratio_cumulative, ci_lower_cumulative, ci_upper_cumulative)

        if(!all(names(target_names) %in% names(df))) {

                unfound_values <- paste(setdiff( names(target_names), colnames(df)),collapse=", ")

                stop(sprintf("Missing variables for: %s. Check input dataframe contains these variables.",unfound_values))
        }

        # Select only necessary columns (in case extra columns in df) and reorder columns so that they are in correct order for the renaming function
        df <- df[,c(level, level_names, win_inherit, win, tie, loss, loss_inherit, win_ratio_individual,
                    ci_lower_individual, ci_upper_individual, win_ratio_cumulative, ci_lower_cumulative, ci_upper_cumulative)]

        # Rename the columns to the necessary names for the plot function below
        colnames(df) <- ifelse(is.na(target_names[colnames(df)]),colnames(df),target_names[colnames(df)])

        ### Parameters for plots -----------------------
        # Set parameters for plot
        outcome_box_width <- 1
        overall_box_width <- 0.6
        forest_plot_polygon_height <- 0.36
        forest_plot_line_offset <- 0.1

        forest_plot_label <- effect_name
        outcome_label <- sprintf("Outcome %s", tolower(effect_name))
        cumulative_label <- sprintf("Cumulative %s", tolower(effect_name))


        # Sort data in ascending level in case dataframe is out of level order
        df <- df[order(df$level),]


        if(overall_line==FALSE) {
                # Create a dataframe with overall data line info
                overall_line_df <- data.frame(level=(max(df$level)+1), level_names="Overall",
                                              win_inherit=0,
                                              win=((unname(unlist(df[df$level==max(df$level),"win_inherit"])))+(unname(unlist(df[df$level==max(df$level),"win"])))),
                                              tie=(unname(unlist(df[df$level==max(df$level),"tie"]))),
                                              loss=((unname(unlist(df[df$level==max(df$level),"loss_inherit"])))+(unname(unlist(df[df$level==max(df$level),"loss"])))),
                                              loss_inherit=0,
                                              win_ratio_individual=(unname(unlist(df[df$level==max(df$level),"win_ratio_cumulative"]))),
                                              ci_lower_individual=(unname(unlist(df[df$level==max(df$level),"ci_lower_cumulative"]))),
                                              ci_upper_individual=(unname(unlist(df[df$level==max(df$level),"ci_upper_cumulative"]))),
                                              win_ratio_cumulative=(unname(unlist(df[df$level==max(df$level),"win_ratio_cumulative"]))),
                                              ci_lower_cumulative=(unname(unlist(df[df$level==max(df$level),"ci_lower_cumulative"]))),
                                              ci_upper_cumulative=(unname(unlist(df[df$level==max(df$level),"ci_upper_cumulative"]))))

                # Bind overall line dataframe to df for plotting
                df <- rbind(df, overall_line_df)
        }



        # Create a list of level names to use later in plot (reverse order given the plot is reversed)
        level_label_names_list <- rev(as.list(df$level_names))

        # Put dataframe in long form to generate proportions of wins/ties/losses etc.
        df_long <- do.call("rbind",lapply(c("loss_inherit","loss","tie","win","win_inherit"), function(i){
               out <-  df[,c("level","level_names",i)]
               colnames(out)[3] <- "n"
               out$pairType <- i
               out
        }))

        # Reorder long form dataframe
        df_long <- df_long[order(df_long$level),]

        # Make pairtype a factor
        df_long$pairType <- factor(df_long$pairType,
                                   levels = c("loss_inherit", "loss", "tie", "win", "win_inherit"),
                                   labels = c("Inherited losses", "Losses", "Ties", "Wins", "Inherited wins"))

        # Create a proportion variable for each level
        df_long$p <- df_long$n
        for(i in unique(df_long$level)){
                df_long[df_long$level == i,"p"] <- df_long[df_long$level == i,"p"]/sum(df_long[df_long$level == i,"p"])
        }

        df_long <- do.call("rbind",by(df_long,df_long$level,function(df_rb){
                df_rb$p_prev <- ((cumsum(df_rb$p)-df_rb$p))
                df_rb
        }))


        # Set box width variable
        df_long$width <- ifelse(df_long$level==max(df_long$level), overall_box_width, outcome_box_width)

        ### Proportional stacked bar graph -----------------------
        # Win ratio plot with separate overall line
        wr_box_overall2 <- ggplot2::ggplot(df_long) +
                ggplot2::geom_rect(
                                ggplot2::aes(
                                        xmin=level-width/2,
                                        xmax=level+width/2,
                                        ymin=p_prev,
                                        ymax=p_prev+p,
                                        fill=pairType
                                        ),
                                color="black"
                                ) +
                ggplot2::scale_x_reverse(
                        breaks=c(max(df_long$level):1),
                        labels=level_label_names_list,
                        minor_break=NULL,
                        limits=c(
                                max(df_long$level)+overall_box_width/2,
                                outcome_box_width/2
                                )
                ) +
                ggplot2::scale_fill_manual(
                        values = c(
                                   "tan1",
                                   "red2",
                                   "khaki1",
                                   "forestgreen",
                                   "#97d42c"
                                   )
                ) +
                ggplot2::guides(
                        fill = ggplot2::guide_legend(
                                title = "Treatment comparison",
                                title.theme = ggplot2::element_text(face = "bold")
                        )
                ) +
                ggplot2::coord_flip() +
                ggplot2::ylab("Proportion (intervention vs control)") +
                ggplot2::theme_bw() +
                ggplot2::theme(
                        panel.grid.major.y = ggplot2::element_blank(),
                        panel.grid.minor.y = ggplot2::element_blank()
                ) +
                ggplot2::theme(axis.title.x = ggplot2::element_text(face="bold"))

        ### Tie handling -----------------------
        if(!is.null(tie_handling)){

                #### Drop ties -----------------------
                if(tie_handling=="drop"){
                        # drop: ties_start + losses/(wins+losses)*ties
                        dash_ties_start <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
                                        "p_prev"
                                ]
                        ))

                        dash_wins <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Wins",
                                        "p"
                                ]
                        ))

                        dash_losses <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Losses",
                                        "p"
                                ]
                        ))

                        dash_ties <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
                                        "p"
                                ]
                        ))

                        dash_x <- max(df_long$level)-overall_box_width/2
                        dash_xend <- max(df_long$level)+overall_box_width/2
                        y_dash <- dash_ties_start + dash_losses/(dash_losses+dash_wins)*dash_ties

                        wr_box_overall2 <- wr_box_overall2 +
                                ggplot2::annotate(
                                        "segment",
                                        y=y_dash,
                                        yend=y_dash,
                                        x=dash_x,
                                        xend=dash_xend,
                                        linetype="dashed"
                                ) +
                                ggplot2::xlab(NULL)
                }

                ### Split ties -----------------------
                if(tie_handling=="split"){

                        # split: ties_start + 0.5*ties
                        dash_ties_start <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
                                        "p_prev"
                                ]
                        ))

                        dash_ties <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
                                        "p"
                                ]
                        ))

                        dash_x <- max(df_long$level)-overall_box_width/2
                        dash_xend <- max(df_long$level)+overall_box_width/2
                        y_dash <- dash_ties_start + 0.5*dash_ties

                        wr_box_overall2 <- wr_box_overall2 +
                                ggplot2::annotate(
                                        "segment",
                                        y=y_dash,
                                        yend=y_dash,
                                        x=dash_x,
                                        xend=dash_xend,
                                        linetype="dashed"
                                ) +
                                ggplot2::xlab(NULL)
                }

        } # End if !is.na(tie_handling)

        if(!is.null(neutral_point)){
                # Create dataframe with info to make the green and red background panels for forest plot
                background_colourpanel <- data.frame(favours_int=c(0,1),
                                                     xmin=c(Inf,Inf),
                                                     xmax=c(-Inf,-Inf),
                                                     ymin=c(-Inf,neutral_point),
                                                     ymax=c(neutral_point,Inf))

                background_colourpanel$favours_int <- factor(background_colourpanel$favours_int,
                                                             levels = c(0,1),
                                                             labels = c("Favours control", "Favours intervention"))
        }



        # Create a long format of the dataframe with just the data for the forest plot
        drop <- c("win_inherit", "win", "tie", "loss", "loss_inherit")
        df_forest_wide <- df[ , !(names(df) %in% drop)]


        df_forest <- do.call("rbind",lapply(c("_individual","_cumulative"),function(i){

                if(i=="_cumulative"){
                        varNames <- c("win_ratio_cumulative", "ci_lower_cumulative", "ci_upper_cumulative")
                } else {
                        varNames <- c("win_ratio_individual", "ci_lower_individual", "ci_upper_individual")
                }

                out <- df_forest_wide[,c("level","level_names",varNames)]
                colnames(out)[3:5] <- c("win_ratio","ci_lower","ci_upper")

                out$outcome_type <- i
                out
        }))

        # Reorder long form dataframe
        df_forest <- df_forest[order(df_forest$level),]


        df_forest$level_offset <- ifelse(df_forest$outcome_type == "_individual",
                                                                df_forest$level-forest_plot_line_offset,
                                                                df_forest$level+forest_plot_line_offset)
        df_forest$outcome_type <- factor(df_forest$outcome_type,
                                         levels = c("_individual", "_cumulative"),
                                         labels = c(outcome_label, cumulative_label))


        ### Forest plot -----------------------
        # Win ratio forest plot with overall level as separate diamond and cumulative win ratio

        # Create forest plot if neutral point is set
        if(!is.null(neutral_point)){
                wr_forest_overall_diamond_cum2 <- ggplot2::ggplot(df_forest) +
                        ggplot2::geom_rect(data=background_colourpanel,
                                           ggplot2::aes(
                                                   xmin=xmin,
                                                   xmax=xmax,
                                                   ymin=ymin,
                                                   ymax=ymax,
                                                   fill=favours_int
                                                   ),
                                           alpha=0.15) +
                        ggplot2::scale_fill_manual(
                                values=c("#D10707",
                                         "#45E928")
                                ) +
                        ggplot2::geom_hline(yintercept=neutral_point,color="#1A2678") +
                        ggplot2::geom_line(data=df_forest[df_forest$level<max(df$level) & df_forest$outcome_type == cumulative_label, ],
                                           ggplot2::aes(
                                                   x=level_offset,
                                                   y=win_ratio
                                           ),
                                           color="black",
                                           linetype="dashed"
                        ) +
                        ggplot2::geom_errorbar(data = df_forest[df_forest$level<max(df_forest$level), ],
                                               ggplot2::aes(ymin=ci_lower, ymax=ci_upper, x=level_offset, color=outcome_type), width=0
                        ) +
                        ggplot2::geom_point(data = df_forest[df_forest$level<max(df_forest$level), ],
                                            ggplot2::aes(y=win_ratio, shape=outcome_type, x=level_offset, color=outcome_type), size=3
                        ) +
                        ggplot2::geom_polygon(
                                data = data.frame(
                                        x_coord=c(max(df_forest$level)-forest_plot_polygon_height/2, max(df_forest$level),
                                                  max(df_forest$level)+forest_plot_polygon_height/2, max(df_forest$level)
                                        ),
                                        y_coord=c(unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "win_ratio"])),
                                                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "ci_upper"])),
                                                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "win_ratio"])),
                                                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "ci_lower"]))
                                        )
                                ),
                                ggplot2::aes(x=x_coord, y=y_coord)
                        ) +
                        ggplot2::scale_x_reverse(
                                breaks=NULL,
                                limits=c(
                                        max(df_forest$level)+overall_box_width/2,
                                        outcome_box_width/2
                                )
                        ) +
                        ggplot2::scale_y_continuous(limits=c(0,NA)) +
                        ggplot2::xlab(NULL) +
                        ggplot2::ylab(forest_plot_label) +
                        ggplot2::guides(
                                color = ggplot2::guide_legend(reverse = FALSE, title = NULL),
                                shape = ggplot2::guide_legend(title = NULL),
                                fill = ggplot2::guide_legend(title = NULL)
                        ) +
                        ggplot2::scale_color_manual(values=c("grey40", "black")) +
                        ggplot2::scale_shape_manual(values=c(4,16)) +
                        ggplot2::coord_flip() +
                        ggplot2::theme_bw() +
                        ggplot2::theme(axis.title.x = ggplot2::element_text(face="bold"))
        }


        # Create forest plot if no neutral point is set
        if(is.null(neutral_point)){
                wr_forest_overall_diamond_cum2 <- ggplot2::ggplot(df_forest) +
                        ggplot2::geom_line(data=df_forest[df_forest$level<max(df$level) & df_forest$outcome_type == cumulative_label, ],
                                           ggplot2::aes(
                                                   x=level_offset,
                                                   y=win_ratio
                                           ),
                                           color="black",
                                           linetype="dashed"
                        ) +
                        ggplot2::geom_errorbar(data = df_forest[df_forest$level<max(df_forest$level), ],
                                               ggplot2::aes(ymin=ci_lower, ymax=ci_upper, x=level_offset, color=outcome_type), width=0
                        ) +
                        ggplot2::geom_point(data = df_forest[df_forest$level<max(df_forest$level), ],
                                            ggplot2::aes(y=win_ratio, shape=outcome_type, x=level_offset, color=outcome_type), size=3
                        ) +
                        ggplot2::geom_polygon(
                                data = data.frame(
                                        x_coord=c(max(df_forest$level)-forest_plot_polygon_height/2, max(df_forest$level),
                                                  max(df_forest$level)+forest_plot_polygon_height/2, max(df_forest$level)
                                        ),
                                        y_coord=c(unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "win_ratio"])),
                                                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "ci_upper"])),
                                                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "win_ratio"])),
                                                  unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "ci_lower"]))
                                        )
                                ),
                                ggplot2::aes(x=x_coord, y=y_coord)
                        ) +
                        ggplot2::scale_x_reverse(
                                breaks=NULL,
                                limits=c(
                                        max(df_forest$level)+overall_box_width/2,
                                        outcome_box_width/2
                                )
                        ) +
                        ggplot2::scale_y_continuous(limits=c(0,NA)) +
                        ggplot2::xlab(NULL) +
                        ggplot2::ylab(forest_plot_label) +
                        ggplot2::guides(
                                color = ggplot2::guide_legend(reverse = FALSE, title = NULL),
                                shape = ggplot2::guide_legend(title = NULL)
                        ) +
                        ggplot2::scale_color_manual(values=c("grey40", "black")) +
                        ggplot2::scale_shape_manual(values=c(4,16)) +
                        ggplot2::coord_flip() +
                        ggplot2::theme_bw() +
                        ggplot2::theme(axis.title.x = ggplot2::element_text(face="bold"))
        }



        ### Patching plots together -----------------------
        patchwork::wrap_plots(wr_box_overall2, wr_forest_overall_diamond_cum2) + patchwork::plot_layout(guides = 'collect')

}


