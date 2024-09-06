# Win ratio plot function
winRatioPlot <- function(df,tie_handling=NA,
                         level="level", level_names="level_names", win_inherit="win_inherit", win="win", tie="tie",
                         loss="loss", loss_inherit="loss_inherit", win_ratio="win_ratio", ci_lower="ci_lower", ci_upper="ci_upper",
                         win_ratio_cum="win_ratio_cum", ci_lower_cum="ci_lower_cum", ci_upper_cum="ci_upper_cum")
{
        # library(tidyverse)
        library(patchwork)

        # Include some checks to ensure that the argument inserted is a dataframe and that the necessary variables
        # are included in the dataframe and stop and report an error message if they are missing
        target_names <- c(level="level", level_names="level_names", win_inherit="win_inherit", win="win", tie="tie",
                          loss="loss", loss_inherit="loss_inherit", win_ratio="win_ratio", ci_lower="ci_lower", ci_upper="ci_upper",
                          win_ratio_cum="win_ratio_cum", ci_lower_cum="ci_lower_cum", ci_upper_cum="ci_upper_cum")
        names(target_names) <- c(level, level_names, win_inherit, win, tie, loss, loss_inherit, win_ratio,
                                 ci_lower, ci_upper, win_ratio_cum, ci_lower_cum, ci_upper_cum)
        colnames(df) <- ifelse(is.na(target_names[colnames(df)]),colnames(df),target_names[colnames(df)])

        expected_names <- c("level", "level_names", "win_inherit", "win", "tie",
                          "loss", "loss_inherit", "win_ratio", "ci_lower", "ci_upper",
                          "loss", "loss_inherit", "win_ratio", "ci_lower", "ci_upper",
                          "win_ratio_cum", "ci_lower_cum", "ci_upper_cum")

        if(length(setdiff(expected_names, colnames(df)) >0)) {
                stop(sprintf("Missing variables for: %s. Check input dataframe contains these variables.",
                             paste(setdiff(expected_names, colnames(df)),collapse=", ")))
        }


        # Set parameters for plot
        outcome_box_width <- 1
        overall_box_width <- 0.6
        forest_plot_polygon_height <- 0.36
        forest_plot_line_offset <- 0.1

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
        for(i in unique(df_long$level)) df_long[df_long$level == i,"p"] <- df_long[df_long$level == i,"p"]/sum(df_long[df_long$level == i,"p"])

        df_long <- do.call("rbind",by(df_long,df_long$level,function(df_rb){
                df_rb$p_prev <- ((cumsum(df_rb$p)-df_rb$p))
                df_rb
        }))


        # Set box width variable
        df_long$width <- ifelse(df_long$level==max(df_long$level), overall_box_width, outcome_box_width)

        # Win ratio plot with separate overall line
        wr_box_overall2 <- ggplot2::ggplot(df_long) +
                geom_rect(aes(xmin=level-width/2,xmax=level+width/2,
                              ymin=p_prev,ymax=p_prev+p,fill=pairType), color="black") +
                scale_x_reverse(breaks=c(max(df_long$level):1),
                                labels=level_label_names_list,
                                minor_break=NULL, limits=c(max(df_long$level)+overall_box_width/2,outcome_box_width/2)) +
                scale_fill_manual(values = c("tan1", "red2", "khaki1", "forestgreen", "#97d42c")) +
                guides(fill = guide_legend(title = "Treatment comparison", title.theme = element_text(face = "bold"))) +
                coord_flip() +
                ylab(expression(bold("Proportion (intervention vs control)"))) +
                theme_bw() +
                theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())


        if(tie_handling=="drop"){
                # drop: ties_start + losses/(wins+losses)*ties
                dash_ties_start <- unname(unlist(df_long[df_long$level==max(df_long$level) & df_long$pairType=="Ties","p_prev"]))
                dash_wins <- unname(unlist(df_long[df_long$level==max(df_long$level) & df_long$pairType=="Wins","p"]))
                dash_losses <- unname(unlist(df_long[df_long$level==max(df_long$level) & df_long$pairType=="Losses","p"]))
                dash_ties <- unname(unlist(df_long[df_long$level==max(df_long$level) & df_long$pairType=="Ties","p"]))

                dash_x <- max(df_long$level)-overall_box_width/2
                dash_xend <- max(df_long$level)+overall_box_width/2
                y_dash <- dash_ties_start + dash_losses/(dash_losses+dash_wins)*dash_ties

                wr_box_overall2 <- wr_box_overall2 + annotate("segment",
                                                              y=y_dash,
                                                              yend=y_dash,
                                                              x=dash_x,
                                                              xend=dash_xend,
                                                              linetype="dashed") + xlab(NULL)
        }

        if(tie_handling=="split"){
                # split: ties_start + 0.5*ties
                dash_ties_start <- unname(unlist(df_long[df_long$level==max(df_long$level) & df_long$pairType=="Ties","p_prev"]))
                dash_ties <- unname(unlist(df_long[df_long$level==max(df_long$level) & df_long$pairType=="Ties","p"]))

                dash_x <- max(df_long$level)-overall_box_width/2
                dash_xend <- max(df_long$level)+overall_box_width/2
                y_dash <- dash_ties_start + 0.5*dash_ties

                wr_box_overall2 <- wr_box_overall2 + annotate("segment",
                                                              y=y_dash,
                                                              yend=y_dash,
                                                              x=dash_x,
                                                              xend=dash_xend,
                                                              linetype="dashed") + xlab(NULL)
        }

        # Create dataframe with info to make the green and red background panels for forest plot
        background_colourpanel <- data.frame(favours_int=c(0,1),
                                             xmin=c((max(df$level)+overall_box_width/2),(max(df$level)+overall_box_width/2)),
                                             xmax=c(outcome_box_width/2,outcome_box_width/2),
                                             ymin=c(0,1),
                                             ymax=c(1,max(df$ci_upper)))

        background_colourpanel$favours_int <- factor(background_colourpanel$favours_int,
                                                     levels = c(0,1),
                                                     labels = c("Favours control", "Favours intervention"))


        # Win ratio forest plot with overall level as separate diamond and cumulative win ratio (annotated version has axis limit adjusted to make extra space)
        wr_forest_overall_diamond_cum2 <- ggplot2::ggplot(df) +
                geom_rect(data=background_colourpanel, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=favours_int), alpha=0.15) +
                scale_fill_manual(values=c("#D10707", "#45E928")) +
                geom_hline(yintercept=1,color="#1A2678") +
                geom_errorbar(data=df[df$level<max(df$level), ],
                              aes(x=level-forest_plot_line_offset, ymin=ci_lower, ymax=ci_upper, color="Outcome win ratio"), width=0) +
                geom_point(data=df[df$level<max(df$level), ],
                           aes(x=level-forest_plot_line_offset, y=win_ratio, color="Outcome win ratio"), size=2.5, shape=4) +
                geom_line(data=df[df$level<max(df$level), ],
                          aes(x=level+forest_plot_line_offset,y=win_ratio_cum), color="black", linetype="dashed") +
                geom_errorbar(data=df[df$level<max(df$level), ],
                              aes(x=level+forest_plot_line_offset, ymin=ci_lower_cum, ymax=ci_upper_cum, color="Cumulative win ratio"), width=0) +
                geom_point(data=df[df$level<max(df$level), ],
                           aes(x=level+forest_plot_line_offset, y=win_ratio_cum, color="Cumulative win ratio"), size=2.5) +
                geom_polygon(data = data.frame(x=c(max(df$level)-forest_plot_polygon_height/2, max(df$level),
                                                   max(df$level)+forest_plot_polygon_height/2, max(df$level)),
                                               y=c(df[df$level==max(df$level), "win_ratio"],
                                                   df[df$level==max(df$level), "ci_upper"],
                                                   df[df$level==max(df$level), "win_ratio"],
                                                   df[df$level==max(df$level), "ci_lower"])),
                             aes(x=x, y=y)) +
                scale_x_reverse(breaks=NULL, limits=c(max(df$level)+overall_box_width/2,outcome_box_width/2)) +
                scale_y_continuous(limits=c(0,NA)) +
                xlab(NULL) +
                ylab(expression(bold("Win ratio"))) +
                guides(color = guide_legend(reverse = TRUE, title = NULL), fill = guide_legend(title = NULL)) +
                scale_color_manual(values=c("black", "grey40")) +
                coord_flip() +
                theme_bw()

        # Combine plots
        wr_box_overall2 + wr_forest_overall_diamond_cum2 + patchwork::plot_layout(guides = 'collect')
}


