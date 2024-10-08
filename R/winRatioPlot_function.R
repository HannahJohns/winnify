#' Visualise prioritised outcome effect estimates
#'
#' Visualises the contribution of individual outcomes under generalized pairwise
#' comparisons with prioritized outcomes methods, e.g. Win Ratio (Pocock 2012),
#' Win Odds (Dong 2020) and Net Benefit (Buyse 2010).
#'
#'
#' @usage
#' winRatioPlot(df, effect_name, neutral_point=NULL,
#' tie_handling=NULL, outcome_names="outcome_names",
#' win_inherit="win_inherit", win="win", tie="tie",
#' loss="loss", loss_inherit="loss_inherit",
#' win_ratio_individual="win_ratio_individual",
#' ci_lower_individual="ci_lower_individual",
#' ci_upper_individual="ci_upper_individual",
#' win_ratio_cumulative="win_ratio_cumulative",
#' ci_lower_cumulative="ci_lower_cumulative",
#' ci_upper_cumulative="ci_upper_cumulative")
#'
#' @param df A data frame containing the analysis results to be plotted.
#' @param effect_name A string specifying type of analysis to appear in the forest plot label.
#' @param neutral_point An number specifying the point of no effect for intervention vs control.
#' @param tie_handling A string specifying how ties should be treated.
#' @param outcome_names A string indicating the column of \code{df} containing the names
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
#' the cumulative win ratio for each outcome when considered ranked.
#' @param ci_lower_cumulative A string indicating the column of \code{df} containing
#' the lower boundary of the confidence interval for the cumulative win ratio for
#' each outcome when considered ranked.
#' @param ci_upper_cumulative A string indicating the column of \code{df} containing
#' the upper boundary of the confidence interval for the cumulative win ratio for
#' each outcome when considered ranked.
#'
#'
#' @details
#' The win ratio plot function is a way to visualise win ratio/win odds/etc. estimates
#' based on prioritised outcomes. This is done with two plots:
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
#' The dataframe \code{df} must contain columns for all parameters listed except
#' \code{effect_name, neutral_point and tie_handling}.
#'
#' Note, the outcome hierarchy is inferred from the order of the rows in the \code{df},
#' whereby the first row is the highest level outcome, etc.
#'
#' \code{tie_handling} specifies how ties are handled in the analysis so that a
#' dashed line at the position ties were assigned to the wins and losses in the overall
#' row can be added. This should be set to \code{"drop"} or \code{"split"}. If not
#' specified no dashed line will appear indicating the position of splitting of ties
#' for the overall level.
#'
#' Note, at each outcome level, \code{win} and \code{loss} correspond to the new wins and
#' losses that were not already calculated from outcomes with greater priority.
#' \code{win_inherit} and \code{loss_inherit} correspond to the cumulative sums of
#' wins and losses that are inherited from higher ranked outcomes.
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
#'
#' @examples
#' # Create a dataframe containing analysis results to be plotted and visualise the prioritised outcome effect estimate
#' test_data_overall_fn <- data.frame(outcome_names=c("Death", "Length of hospital stay", "Time off work", "Wellbeing"),
#'                                 win_inherit=c(0,15,25,33),
#'                                 win=c(15,10,8,15),
#'                                 tie=c(80,60,40,15),
#'                                 loss=c(5,10,12,10),
#'                                 loss_inherit=c(0,5,15,27),
#'                                 win_ratio=c(3,1,0.67,1.5),
#'                                 ci_lower=c(2.6,0.6,0.27,1.1),
#'                                 ci_upper=c(3.4,1.4,1.07,1.9),
#'                                 win_ratio_cum=c(3,1.67,1.22,1.30),
#'                                 ci_lower_cum=c(2.6,1.32,0.92,1.10),
#'                                 ci_upper_cum=c(3.4,2.02,1.52,1.50))
#' WinRatioPlot::winRatioPlot(test_data_overall_fn, effect_name = "Win ratio", neutral_point = 1,
#'                         outcome_names="outcome_names", win_inherit="win_inherit", win="win", tie="tie",
#'                         loss="loss", loss_inherit="loss_inherit",
#'                         win_ratio_individual="win_ratio", ci_lower_individual="ci_lower", ci_upper_individual="ci_upper",
#'                         win_ratio_cumulative="win_ratio_cum", ci_lower_cumulative="ci_lower_cum", ci_upper_cumulative="ci_upper_cum")
#'
#' @references
#' Pocock SJ, Ariti CA, Collier TJ, Wang D. The win ratio: a new approach to the analysis of composite endpoints in clinical trials based on clinical priorities. European Heart Journal. 2012;33(2):176-182. doi:10.1093/eurheartj/ehr352
#'
#' Dong G, Hoaglin DC, Qiu J, et al. The Win Ratio: On Interpretation and Handling of Ties. Statistics in Biopharmaceutical Research. 2020;12(1):99-106. doi:10.1080/19466315.2019.1575279
#'
#' Buyse M. Generalized pairwise comparisons of prioritized outcomes in the two-sample problem. Statistics in Medicine. 2010;29(30):3245-3257. doi:10.1002/sim.3923
#'
#'
winRatioPlot <- function(df, effect_name, neutral_point=NULL, tie_handling=NULL,
                         outcome_names="outcome_names", win_inherit="win_inherit", win="win", tie="tie",
                         loss="loss", loss_inherit="loss_inherit",
                         win_ratio_individual="win_ratio_individual", ci_lower_individual="ci_lower_individual", ci_upper_individual="ci_upper_individual",
                         win_ratio_cumulative="win_ratio_cumulative", ci_lower_cumulative="ci_lower_cumulative", ci_upper_cumulative="ci_upper_cumulative")
{
        # Include some checks to ensure that the argument inserted is a dataframe and that the necessary variables
        # are included in the dataframe and stop and report an error message if they are missing
        target_names <- c(outcome_names="outcome_names", win_inherit="win_inherit", win="win", tie="tie",
                          loss="loss", loss_inherit="loss_inherit",
                          win_ratio="win_ratio_individual", ci_lower="ci_lower_individual", ci_upper="ci_upper_individual",
                          win_ratio_cum="win_ratio_cumulative", ci_lower_cum="ci_lower_cumulative", ci_upper_cum="ci_upper_cumulative")

        names(target_names) <- c(outcome_names, win_inherit, win, tie, loss, loss_inherit, win_ratio_individual,
                                 ci_lower_individual, ci_upper_individual, win_ratio_cumulative, ci_lower_cumulative, ci_upper_cumulative)

        if(!all(names(target_names) %in% names(df))) {

                unfound_values <- paste(setdiff(names(target_names), colnames(df)),collapse=", ")

                stop(sprintf("Missing variables for: %s. Check input dataframe contains these variables.",unfound_values))
        }


        # Select only necessary columns (in case extra columns in df) and reorder columns so that they are in correct order for the renaming function
        df <- df[,c(outcome_names, win_inherit, win, tie, loss, loss_inherit, win_ratio_individual,
                    ci_lower_individual, ci_upper_individual, win_ratio_cumulative, ci_lower_cumulative, ci_upper_cumulative)]

        # Rename the columns to the necessary names for the plot function below
        colnames(df) <- ifelse(is.na(target_names[colnames(df)]),colnames(df),target_names[colnames(df)])

        # Create level ranking order variable
        df <- cbind(level = 1:nrow(df), df)

        # Create a dataframe with overall data line info
        overall_line_df <- data.frame(level=(max(df$level)+1), outcome_names="Overall",
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


        ### Parameters for plots -----------------------
        # Set parameters for plot
        outcome_box_width <- 1
        overall_box_width <- 0.6
        forest_plot_polygon_height <- 0.36
        forest_plot_line_offset <- 0.1

        forest_plot_label <- effect_name
        outcome_label <- sprintf("Outcome %s", tolower(effect_name))
        cumulative_label <- sprintf("Cumulative %s", tolower(effect_name))


        # Create a list of outcome names to use later in plot (reverse order given the plot axis is reversed)
        level_label_names_list <- rev(as.list(df$outcome_names))

        # Put dataframe in long form to generate proportions of wins/ties/losses etc.
        df_long <- do.call("rbind",lapply(c("loss_inherit","loss","tie","win","win_inherit"), function(i){
               out <-  df[,c("level","outcome_names",i)]
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

                out <- df_forest_wide[,c("level","outcome_names",varNames)]
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


