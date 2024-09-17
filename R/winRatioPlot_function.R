#' Visualise prioritised outcome effect estimates
#'
#' Visualises the contribution of individual outcomes under the generalized
#' pairwise comparisons with prioritized outcomes, e.g. Win Ratio (CITE), Win Odds (CITE) and
#' Net Benefit (Buyse 2010).
#' The win ratio plot function is a way to visualise win ratio (or win odds) data, which incorporates
#' ranking outcomes in order of importance and then assessing pairwise comparisons of wins/losses/ties
#' for the intervention vs the control treatment. A "win" is recorded if the intervention participant
#' did better on the outcome than the pairwise control participant, a "tie" is recorded if the intervention
#' participant did the same on the outcome than the pairwise control participant, and a "loss" is recorded
#' if the intervention participant did worse on the outcome than the pairwise control participant.
#'
#' @usage
#' winRatioPlot(df,type=NA, tie_handling=NA,
#' level="level", level_names="level_names", win_inherit="win_inherit", win="win", tie="tie",
#' loss="loss", loss_inherit="loss_inherit",
#' win_ratio_individual="win_ratio_individual", ci_lower_individual="ci_lower_individual",
#' ci_upper_individual="ci_upper_individual",
#' win_ratio_cumulative="win_ratio_cumulative", ci_lower_cumulative="ci_lower_cumulative",
#' ci_upper_cumulative="ci_upper_cumulative")
#'
#' @param df A data frame containing win ratio analysis results to be plotted. Must contain variables for all the arguments (excluding tie_handling). See details section for more information.
#' @param type A string specifying type of analysis. Should be equal to "wr" for win ratio or "or" for odds ratio or ...
#' @param tie_handling A string specifying how ties should be treated. Should be equal to "drop" or "split" or NA.
#' @param level A string indicating the column of \code{df} containing the ranking of the outcomes. The data within the variable should be integers and the order should be from "1" for the first level outcome, "2" for the second level outcome etc.
#' @param level_names A string indicating the column of \code{df} containing the names of the outcomes.
#' @param win_inherit A string indicating the column of \code{df} containing the inherited wins for each outcome. The data within the variable should be integers and contain inherited wins data for outcomes when ranked.
#' @param win A string indicating the column of \code{df} containing the wins for each outcome. The data within the variable should be integers and contain wins data for outcomes when ranked.
#' @param tie A string indicating the column of \code{df} containing the ties for each outcome. The data within the variable should be integers and contain ties data for outcomes when ranked.
#' @param loss A string indicating the column of \code{df} containing the losses for each outcome. The data within the variable should be integers and contain losses data for outcomes when ranked.
#' @param loss_inherit A string indicating the column of \code{df} containing the inherited losses for each outcome. The data within the variable should be integers and contain inherited losses data for outcomes when ranked.
#' @param win_ratio_individual A string indicating the column of \code{df} containing the win ratio for each outcome when considered independently (not ranked). The data within the variable should be numeric and contain win ratio data for outcomes when considered independently.
#' @param ci_lower_individual A string indicating the column of \code{df} containing the lower boundary of the confidence interval for the win ratio for each outcome when considered independently (not ranked). The data within the variable should be numeric and contain the lower boundary of the confidence interval of the win ratio data for outcomes when considered independently.
#' @param ci_upper_individual A string indicating the column of \code{df} containing the upper boundary of the confidence interval for the win ratio for each outcome when considered independently (not ranked). The data within the variable should be numeric and contain the upper boundary of the confidence interval of the win ratio data for outcomes when considered independently.
#' @param win_ratio_cumulative A string indicating the column of \code{df} containing the win ratio for each outcome when considered ranked. The data within the variable should be numeric and contain win ratio data for outcomes when considered ranked.
#' @param ci_lower_cumulative A string indicating the column of \code{df} containing the lower boundary of the confidence interval for the win ratio for each outcome when considered ranked. The data within the variable should be numeric and contain the lower boundary of the confidence interval of the win ratio data for outcomes when considered ranked.
#' @param ci_upper_cumulative A string indicating the column of \code{df} containing the upper boundary of the confidence interval for the win ratio for each outcome when considered ranked. The data within the variable should be numeric and contain the upper boundary of the confidence interval of the win ratio data for outcomes when considered ranked.
#'
#' @details
#'
#'
#'
#' @return
#' A \code{patchwork} object containing two plots (Left and Right):
#' \describe{
#' \item The left plot shows the proportions of inherited wins/wins/ties/losses/inherited
#' losses for each outcome when ranked and overall.
#' \item The right plot shows the measured effect (e.g., win ratio) at both a ranked outcome
#' level (with overall) and for each outcome independently.
#' }
#'
#'
#'
#'
#'@references
#' Buyse M. Generalized pairwise comparisons of prioritized outcomes in the two-sample problem. Stat Med. 2010 Dec 30;29(30):3245-57. doi: 10.1002/sim.3923. PMID: 21170918.
#'
#'
winRatioPlot <- function(df,type=NA, tie_handling=NA,
                         level="level", level_names="level_names", win_inherit="win_inherit", win="win", tie="tie",
                         loss="loss", loss_inherit="loss_inherit",
                         win_ratio_individual="win_ratio_individual", ci_lower_individual="ci_lower_individual", ci_upper_individual="ci_upper_individual",
                         win_ratio_cumulative="win_ratio_cumulative", ci_lower_cumulative="ci_lower_cumulative", ci_upper_cumulative="ci_upper_cumulative")
{

        # Include some checks to ensure that the argument inserted is a dataframe and that the necessary variables
        # are included in the dataframe and stop and report an error message if they are missing
        target_names <- c(level="level", level_names="level_names", win_inherit="win_inherit", win="win", tie="tie",
                          loss="loss", loss_inherit="loss_inherit",
                          win_ratio="win_ratio_individual", ci_lower="ci_lower_individual", ci_upper="ci_upper_individual",
                          win_ratio_cum="win_ratio_cumulative", ci_lower_cum="ci_lower_cumulative", ci_upper_cum="ci_upper_cumulative")
        names(target_names) <- c(level, level_names, win_inherit, win, tie, loss, loss_inherit, win_ratio_individual,
                                 ci_lower_individual, ci_upper_individual, win_ratio_cumulative, ci_lower_cumulative, ci_upper_cumulative)
        colnames(df) <- ifelse(is.na(target_names[colnames(df)]),colnames(df),target_names[colnames(df)])

        expected_names <- c("level", "level_names", "win_inherit", "win", "tie",
                          "loss", "loss_inherit", "win_ratio_individual", "ci_lower_individual", "ci_upper_individual",
                          "win_ratio_cumulative", "ci_lower_cumulative", "ci_upper_cumulative")

        if(length(setdiff(expected_names, colnames(df)) >0)) {
                stop(sprintf("Missing variables for: %s. Check input dataframe contains these variables.",
                             paste(setdiff(expected_names, colnames(df)),collapse=", ")))
        }


        ### Parameters for plots -----------------------
        # Set parameters for plot
        outcome_box_width <- 1
        overall_box_width <- 0.6
        forest_plot_polygon_height <- 0.36
        forest_plot_line_offset <- 0.1

        if(!is.na(type)){
                if (type=="wr") {
                        forest_plot_label <- "Win ratio"
                        outcome_label <- "Outcome win ratio"
                        cumulative_label <- "Cumulative win ratio"
                        null_effect_line <- 1
                        }

                if (type=="or") {
                        forest_plot_label <- "Odds ratio"
                        outcome_label <- "Outcome odds ratio"
                        cumulative_label <- "Cumulative odds ratio"
                        null_effect_line <- 1
                        }
                }


        # Sort data in ascending level in case dataframe is out of level order
        df <- df[order(df$level),]

        # Create a list of level names to use later in plot (reverse order given the plot is reversed)
        level_label_names_list <- rev(as.list(df$level_names))


        # Put dataframe in long form
        df_long <- tidyr::pivot_longer(df,
                                c(loss_inherit,loss,tie,win,win_inherit),
                                names_to = "pairType",values_to = "n")

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
        if(!is.na(tie_handling)){

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


        # Create dataframe with info to make the green and red background panels for forest plot
        background_colourpanel <- data.frame(favours_int=c(0,1),
                                             xmin=c(Inf,Inf),
                                             xmax=c(-Inf,-Inf),
                                             ymin=c(-Inf,null_effect_line),
                                             ymax=c(null_effect_line,Inf))

        background_colourpanel$favours_int <- factor(background_colourpanel$favours_int,
                                                     levels = c(0,1),
                                                     labels = c("Favours control", "Favours intervention"))


        # Create a long format of the dataframe with just the data for the forest plot
        drop <- c("win_inherit", "win", "tie", "loss", "loss_inherit")
        df_forest_wide <- df[ , !(names(df) %in% drop)]

        df_forest <- tidyr::pivot_longer(df_forest_wide,
                                  cols = 3:8,
                                  names_pattern = "(.*)(_individual|_cumulative)$",
                                  names_to = c(".value", "outcome_type")
        )

        df_forest$level_offset <- ifelse(df_forest$outcome_type == "_individual",
                                                                df_forest$level-forest_plot_line_offset,
                                                                df_forest$level+forest_plot_line_offset)
        df_forest$outcome_type <- factor(df_forest$outcome_type,
                                         levels = c("_individual", "_cumulative"),
                                         labels = c(outcome_label, cumulative_label))

        ### Forest plot -----------------------
        # Win ratio forest plot with overall level as separate diamond and cumulative win ratio
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
                ggplot2::geom_hline(yintercept=null_effect_line,color="#1A2678") +
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

        ### Patching plots together -----------------------
        patchwork::wrap_plots(wr_box_overall2, wr_forest_overall_diamond_cum2) + patchwork::plot_layout(guides = 'collect')

}


