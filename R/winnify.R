
as_winnify <- function (x, ...) UseMethod("as_winnify", x)

#' Enables Prioritised Outcomes analysis using rank-based statistical methods
#'
#' @description Wrapper function facilitating use of other R analysis functions
#' under the Generalised Pairwise Comparisons framework
#'
#' @usage winnify(formula, statistic, data)
#'
#' @param formula An object of class formula describing the analyses to run. Details of model specification are given in Details
#' @param statistic A function for performing analysis based on a formula and data. Details are given in Details.
#' @param data A data.frame to analyse
#'
#' @details
#' The left hand side of the formula should list all outcomes to consider, in order of decreasing importance from the left.
#' The right hand side of the formula lists all independent variables. The first term on the left hand side indicates the
#' primary variable of interest, which should contain exactly two groups.
#'
#' statistic should be a function that takes a formula and a dataset and should return a named vector of length 4 containing
#' estimate, lower, upper and pval
#'
#' NOTE: wins/losses/ties are not adjusted for covariates even if statistic specifies this
#'
#' @examples
#'
#' statistic <- function(formula,data){
#'
#'         fit <- genodds::genodds(response = data[,as.character(formula)[2]],
#'                                 group = data[,as.character(formula)[3]])
#'
#'         out <- c(fit$pooled_lnodds,fit$pooled_lnconf.int, fit$pooled_p)
#'         names(out) <- c("estimate","lower","upper","pval")
#'         out[1:3] <- exp(out[1:3])
#'         out
#' }
#'
#' result <- winnify(mRS+early_neuro_improvement~treat,
#'                   statistic = statistic,
#'                   data = neuro_improvement)
#'
#' plot(result,neutral_point = 1,log_scale = F)
#'
#'
#'
#'
winnify <- function(formula, statistic, data){

        warning("There are almost no checks in here. This needs fixed.")

        lhs <- formula[-3]
        rhs <- formula[-2]

        get_wlt <- function(y, x){

                xtab <- table(y,x)

                pairTab <- outer(xtab[,1],xtab[,2])

                wins <- sum(pairTab[upper.tri(pairTab,diag = F)])
                losses <- sum(pairTab[lower.tri(pairTab,diag = F)])
                ties <- sum(diag(pairTab))

                return(c(win=wins,tie=ties,loss=losses))
        }

        # We need to modify the data sheet behind the scenes to add in our column
        # for left hand side of formula. This is a little bit cursed but it will
        # identify a safe column name for us to use
        lhs_name <- "lhs_winnified"
        lhs_name_attempt <- 1
        while(lhs_name %in% colnames(data)){
                lhs_name <- paste0("lhs_winnified",lhs_name_attempt)
                lhs_name_attempt <- lhs_name_attempt+1
        }

        # We need to know the group intervention in order to calculate wins/losses/ties
        main_comparator <- attr(terms(rhs),"term.labels")[1]

        # We expect that the main comparator is a factor of two levels, with the second level
        # being intervention.
        data[,main_comparator] <- as.factor(data[,main_comparator])
        if(length(levels(data[,main_comparator])) != 2) stop(sprintf("Main comparator %s must have exactly two levels",main_comparator))

        lhs <- attr(terms(lhs),"term.labels")

        formula <- reformulate(attr(terms(rhs),"term.labels"),response = lhs_name)


        out <- lapply(1:length(lhs), function(i){

                print(i)

                # Run on individual values
                tmp_data <- data
                tmp_data[,lhs_name] <- tmp_data[,lhs[i]]

                individual_result <- statistic(formula,tmp_data)

                # Construct ranks based on cumulative results and run on that

                ranks <- data[,lhs[1:i],drop=F]
                ranks <- unique(ranks)

                ranks <- ranks[
                        do.call("order",c(lapply(1:ncol(ranks),function(i){
                                ranks[,i]
                        })))
                        ,
                        ,drop=F
                ]

                ranks[,lhs_name] <- 1:nrow(ranks)

                tmp_data <- merge(data,ranks)

                cumulative_wlt <- get_wlt(tmp_data[,lhs_name],tmp_data[,main_comparator])
                cumulative_wlt <- c(win_inherit =0, cumulative_wlt,loss_inherit =0) # Just initialise, we will set this up later
                cumulative_result <- statistic(formula,tmp_data)

                # Construct row for object based on these results
                names(individual_result) <- paste(names(individual_result),"individual",sep="_")

                names(cumulative_result) <- paste(names(cumulative_result),"cumulative",sep="_")

                c(cumulative_wlt,cumulative_result,individual_result)

        })

        out <- as.data.frame(do.call("rbind",out))

        out <- cbind(outcome_names = lhs,out)

        # TODO: Discuss with Nat. This doesn't match with what we're doing in other functions, see the following code chunk:
        # data.frame(outcome_names=names(tmp_wr_cumulative$call$outcome[i]),
        #            win_inherit=(tmp_wr_cumulative$total.wins-tmp_wr_cumulative$wins[i]), win=tmp_wr_cumulative$wins[i],
        #            tie=tmp_wr_cumulative$total.ties,
        #            loss=tmp_wr_cumulative$loss[i], loss_inherit=(tmp_wr_cumulative$total.loss-tmp_wr_cumulative$loss[i]),
        #            win_ratio_individual=tmp_wr_individual$wr,
        #            ci_lower_individual=tmp_wr_individual$wr.lower, ci_upper_individual=tmp_wr_individual$wr.upper,
        #            win_ratio_cumulative=tmp_wr_cumulative$wr,
        #            ci_lower_cumulative=tmp_wr_cumulative$wr.lower, ci_upper_cumulative=tmp_wr_cumulative$wr.upper
        # )

        if(nrow(out)>1){
                out$win_inherit[-1] <- out$win[-nrow(out)]
                out$loss_inherit[-1] <- out$loss[-nrow(out)]

                out$win <- out$win - out$win_inherit
                out$loss <- out$loss - out$loss_inherit
        }


        overall_row <- out[nrow(out),,drop=FALSE]

        overall_row[,c("estimate_individual","lower_individual","upper_individual","pval_individual")] <- overall_row[,c("estimate_cumulative","lower_cumulative","upper_cumulative","pval_cumulative")]
        overall_row$outcome_names <- "Overall"
        out <- rbind(out,overall_row)

        attr(out,"levels") <- levels(data[,main_comparator])
        class(out) <- c("Winnify",class(out))

        out

}


#' Visualise prioritised outcome effect estimates
#'
#' Visualises the contribution of individual outcomes under generalized pairwise
#' comparisons with prioritized outcomes methods, e.g. Win Ratio (Pocock 2012),
#' Win Odds (Dong 2020) and Net Benefit (Buyse 2010).
#'
#'
#' @usage
#' po_plot(df, effect_name, neutral_point=NULL,
#' tie_handling=NULL, outcome_names="outcome_names",
#' win_inherit="win_inherit", win="win", tie="tie",
#' loss="loss", loss_inherit="loss_inherit",
#' effect_individual="effect_individual",
#' ci_lower_individual="ci_lower_individual",
#' ci_upper_individual="ci_upper_individual",
#' effect_cumulative="effect_cumulative",
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
#' @param effect_individual A string indicating the column of \code{df} containing
#' the effect size for each outcome when considered independently (not ranked).
#' @param ci_lower_individual A string indicating the column of \code{df} containing
#' the lower boundary of the confidence interval for the effect size for each outcome
#' when considered independently (not ranked).
#' @param ci_upper_individual A string indicating the column of \code{df} containing
#' the upper boundary of the confidence interval for the effect size for each outcome
#' when considered independently (not ranked).
#' @param effect_cumulative A string indicating the column of \code{df} containing
#' the cumulative effect size for each outcome when considered ranked.
#' @param ci_lower_cumulative A string indicating the column of \code{df} containing
#' the lower boundary of the confidence interval for the cumulative effect size for
#' each outcome when considered ranked.
#' @param ci_upper_cumulative A string indicating the column of \code{df} containing
#' the upper boundary of the confidence interval for the cumulative effect size for
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
#' # Create a dataframe containing analysis results to be plotted
#' test_data_overall_fn <- data.frame(outcome_names=c("Death", "Length of hospital stay",
#'                                 "Time off work", "Wellbeing"),
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
#'
#' # Visualise the prioritised outcome effect estimate
#' po_plot(test_data_overall_fn, effect_name = "Win Ratio", neutral_point = 1,
#'                         outcome_names="outcome_names", win_inherit="win_inherit",
#'                         win="win", tie="tie", loss="loss", loss_inherit="loss_inherit",
#'                         effect_individual="win_ratio", ci_lower_individual="ci_lower",
#'                         ci_upper_individual="ci_upper", effect_cumulative="win_ratio_cum",
#'                         ci_lower_cumulative="ci_lower_cum", ci_upper_cumulative="ci_upper_cum")
#'
#' @references
#' Pocock SJ, Ariti CA, Collier TJ, Wang D. The win ratio: a new approach to the analysis of composite endpoints in clinical trials based on clinical priorities. European Heart Journal. 2012;33(2):176-182. doi:10.1093/eurheartj/ehr352
#'
#' Dong G, Hoaglin DC, Qiu J, et al. The Win Ratio: On Interpretation and Handling of Ties. Statistics in Biopharmaceutical Research. 2020;12(1):99-106. doi:10.1080/19466315.2019.1575279
#'
#' Buyse M. Generalized pairwise comparisons of prioritized outcomes in the two-sample problem. Statistics in Medicine. 2010;29(30):3245-3257. doi:10.1002/sim.3923
#'
#'
plot.Winnify <- function(x, log_scale=F,
                         effect_name=NULL,neutral_point=NULL, tie_handling=NULL
                    ){

        # Load default arguments from attr(x) where possible

        # Some arguments need a default, others can just be left as NULL
        if(is.null(effect_name)) {
                effect_name <- attr(x,"effect_name")
        }
        if(is.null(effect_name)) {
                effect_name <-  "Effect Size"
        }



        if(is.null(neutral_point)) {
                neutral_point <- attr(x,"neutral_point")
        }

        if(is.null(tie_handling)) {
                tie_handling <- attr(x,"tie_handling")
        }



        # Create level ranking order variable
        # This is used to help rearrange things later on
        x <- cbind(level = 1:nrow(x), x)

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
        level_label_names_list <- rev(as.list(x$outcome_names))

        # Put dataframe in long form to generate proportions of wins/ties/losses etc.
        df_long <- do.call("rbind",lapply(c("loss_inherit","loss","tie","win","win_inherit"), function(i){
                out <-  x[,c("level","outcome_names",i)]
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
                        color="black",
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

        # Create a long format of the dataframe with just the data for the forest plot
        drop <- c("win_inherit", "win", "tie", "loss", "loss_inherit")
        df_forest_wide <- x[ , !(names(x) %in% drop)]


        df_forest <- do.call("rbind",lapply(c("_individual","_cumulative"),function(i){

                if(i=="_cumulative"){
                        varNames <- c("estimate_cumulative", "lower_cumulative", "upper_cumulative")
                } else {
                        varNames <- c("estimate_individual", "lower_individual", "upper_individual")
                }

                out <- df_forest_wide[,c("level","outcome_names",varNames)]
                colnames(out)[3:5] <- c("estimate","ci_lower","ci_upper")

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


        # We need the final outcome to draw the final overall estimate as a diamond later on.
        # Extract this information here
        diamond_df = data.frame(
                x_coord=c(max(df_forest$level)-forest_plot_polygon_height/2, max(df_forest$level),
                          max(df_forest$level)+forest_plot_polygon_height/2, max(df_forest$level)
                ),
                y_coord=c(unname(unlist(df_forest[df_forest$level==max(df_forest$level) &df_forest$outcome_type==outcome_label, "estimate"])),
                          unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "ci_upper"])),
                          unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "estimate"])),
                          unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "ci_lower"]))
                )
        )



        ### Forest plot -----------------------
        # Win ratio forest plot with overall level as separate diamond and cumulative win ratio

        wr_forest_overall_diamond_cum2 <- ggplot2::ggplot(df_forest)

        if(!is.null(neutral_point)){
                # Create dataframe with info to make the green and red background panels for forest plot
                background_colourpanel <- data.frame(favours_int=c(0,1),
                                                     xmin=c(Inf,Inf),
                                                     xmax=c(-Inf,-Inf),
                                                     ymin=c( ifelse(log_scale,0,-Inf), neutral_point),
                                                     ymax=c(neutral_point,Inf))

                background_colourpanel$favours_int <- factor(background_colourpanel$favours_int,
                                                             levels = c(0,1),
                                                             labels = c("Favours control", "Favours intervention"))

                wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2 +
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
                        )+
                        ggplot2::geom_hline(yintercept=neutral_point,color="#1A2678")
        }



        # Only show path for the cumulative effect, this is the thing we care about
        wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2+
        ggplot2::geom_line(data=df_forest[
                                        df_forest$level<max(x$level) &
                                        df_forest$outcome_type == cumulative_label,
                                        ],
                           ggplot2::aes(
                                   x=level_offset,
                                   y=estimate
                           ),
                           color="black",
                           linetype="dashed"
        )

        # Show error bars and points for both cumulative and individual estimates
        wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2+
        ggplot2::geom_errorbar(data = df_forest[
                                        df_forest$level<max(df_forest$level),
                                      ],
                                  ggplot2::aes(
                                               ymin=ci_lower,
                                               ymax=ci_upper,
                                               x=level_offset,
                                               color=outcome_type
                                               ),
                               width=0
        ) +
        ggplot2::geom_point(data = df_forest[
                                        df_forest$level<max(df_forest$level),
                                   ],
                            ggplot2::aes(
                                    y=estimate,
                                    shape=outcome_type,
                                    x=level_offset,
                                    color=outcome_type),
                            size=3
        )

        wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2+
        ggplot2::geom_polygon(
                data = diamond_df,
                ggplot2::aes(x=x_coord, y=y_coord),group=1
        )


        wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2+
        ggplot2::scale_x_reverse(
                breaks=NULL,
                limits=c(
                        max(df_forest$level)+overall_box_width/2,
                        outcome_box_width/2
                )
        ) +
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


        if(log_scale){
                wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2 + ggplot2::scale_y_log10()
        } else {
                wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2 + ggplot2::scale_y_continuous()
        }

        ### Patching plots together -----------------------
        patchwork::wrap_plots(wr_box_overall2, wr_forest_overall_diamond_cum2) + patchwork::plot_layout(guides = 'collect')

}
