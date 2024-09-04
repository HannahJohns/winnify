
# Use a name like plot.x - what we're doing is producing what's called an S3 generic function
# for the winratio plot.
# https://adv-r.hadley.nz/s3.html

# Win ratio plot function AND COLOURED BACKGROUND reordered for red on left and green on right
plot.WinRatio <- function(x)
{
        library(tidyverse)
        library(patchwork)

        # Extract data and create dataframe
        wr_output_list <- lapply(1:length(wr$call$outcomes), function(i) {

                # Get estimates for win ratio based on outcome i and outcomes 1 to i

                tmp_wr_individual <- winratio(id = wr$call$id, trt = wr$call$trt, active = wr$call$active,
                                              outcomes = wr$call$outcomes[i],
                                              fu = wr$call$fu, data = wr$call$data)

                tmp_wr_cumulative <- winratio(id = wr$call$id, trt = wr$call$trt, active = wr$call$active,
                                outcomes = wr$call$outcomes[1:i],
                                fu = wr$call$fu, data = wr$call$data)

                # TODO: what if the outcomes aren't called outc1?
                # You can access the i-th element of the outcome list using outcome[[i]]

                # tmp_wr_cumulative$call$outcome$outc1[i] also isn't getting the level name - see how your code returns death, then s then t2death
                # If you had a fourth level in the plot it'd crash because there's only three elements in the list

                # based on ?winratio, the name you're looking for is going to be the first element of outcome[[i]],

                # which is outcome[[i]][[1]]
                # For continuous and survival data, this will be a list of values - you'll need to collapse it
                # down to a single value by doing something like:

                # level_name <- outcome[[i]][[1]]
                # if(length(level_name)>1) level_name <- paste(level_name,collapse=",")

                # and then use the newly created level_name variable in the call below
                data.frame(level=i, level_names=tmp_wr_cumulative$call$outcome$outc1[i],
                           win_inherit=(tmp_wr_cumulative$total.wins-tmp_wr_cumulative$wins[i]), win=tmp_wr_cumulative$wins[i],
                           tie=tmp_wr_cumulative$total.ties,
                           loss=tmp_wr_cumulative$loss[i], loss_inherit=(tmp_wr_cumulative$total.loss-tmp_wr_cumulative$loss[i]),
                           win_ratio=(tmp_wr_cumulative$wins[i]/tmp_wr_cumulative$loss[i]),
                           ci_lower=tmp_wr_individual$wr.lower, ci_upper=tmp_wr_individual$wr.upper,
                           win_ratio_cum=tmp_wr_cumulative$wr,
                           ci_lower_cum=tmp_wr_cumulative$wr.lower, ci_upper_cum=tmp_wr_cumulative$wr.upper
                           )
        }
        )

        wr_dataframe <- do.call("rbind",wr_output_list)

        # Create a dataframe with overall data line info
        overall_line <- data.frame(level=(length(wr$call$outcomes)+1), level_names="Overall", win_inherit=0, win=wr$total.wins,
                                   tie=wr$total.ties, loss=wr$total.loss, loss_inherit=0,
                                   win_ratio=wr$wr, ci_lower=wr$wr.lower, ci_upper=wr$wr.upper,
                                   win_ratio_cum=wr$wr, ci_lower_cum=wr$wr.lower, ci_upper_cum=wr$wr.upper)

        df <- rbind(wr_dataframe, overall_line)



        # TODO: You shouldn't need to write plotting code here!
        # Once you have your data frame in the appropriate format, just run the following

        # WinRatioPlot::winRatioPlot(df)

        # and end the function. This will mean that you only have one copy of the plotting code
        # that needs to be maintained, and if we want to make improvements to the plot we just do it there.

        # It also means that if there was another package we wanted to add plotting ability for, we just have to write
        # a new function for it like what we've done here here.




        # # Include some checks to ensure that the argument inserted is a dataframe and that the necessary variables
        # # are included in the dataframe and stop and report an error message if they are missing or named differently
        # if(class(df) != "data.frame"){
        #         stop("Incorrect argument class: dataframe argument must be a dataframe")
        # }
        # if(("level" %in% names(df)) != TRUE){
        #         stop("Missing or different variable name: check level variable is labelled 'level'")
        # }
        # if(("level_names" %in% names(df)) != TRUE){
        #         stop("Missing or different variable name: check level name variable is labelled 'level_names'")
        # }
        # if(("win" %in% names(df)) != TRUE){
        #         stop("Missing or different variable name: check win variable is labelled 'win'")
        # }


        # Sort data in ascending level in case dataframe is out of level order
        df <- df[order(df$level),]




        # Create a list of level names to use later in plot (reverse order given the plot is reversed)
        level_label_names_list <- rev(as.list(df$level_names))

        df_long <- pivot_longer(df,
                                c(loss_inherit,loss,tie,win,win_inherit),
                                names_to = "pairType",values_to = "n")

        df_long$pairType <- factor(df_long$pairType,
                                   levels = c("loss_inherit", "loss", "tie", "win", "win_inherit"),
                                   labels = c("Inherited losses", "Losses", "Ties", "Wins", "Inherited wins"))

        df_long$p <- df_long$n

        for(i in unique(df_long$level)) df_long[df_long$level == i,"p"] <- df_long[df_long$level == i,"p"]/sum(df_long[df_long$level == i,"p"])

        df_long <- do.call("rbind",by(df_long,df_long$level,function(df_rb){
                df_rb$p_prev <- ((cumsum(df_rb$p)-df_rb$p))
                df_rb
        }))


        # Set box width
        df_long$width <- ifelse(df_long$level==max(df_long$level), 0.6,1)






        # Win ratio plot with no space between levels and overall line
        wr_box_overall2 <- ggplot(df_long) +
                geom_rect(aes(xmin=level-width/2,xmax=level+width/2,
                              ymin=p_prev,ymax=p_prev+p,fill=pairType), color="black") +
                scale_x_reverse(breaks=c(max(df_long$level):1),
                                labels=level_label_names_list,
                                minor_break=NULL, limits=c(max(df_long$level)+0.3,0.5)) +
                scale_fill_manual(values = c("tan1", "red2", "khaki1", "forestgreen", "#97d42c")) +
                guides(fill = guide_legend(title = "Treatment comparison", title.theme = element_text(face = "bold"))) +
                coord_flip() +
                ylab(expression(bold("Proportion (intervention vs control)"))) +
                theme_bw() +
                theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

        background_colourpanel <- data.frame(favours_int=c(0,1),
                                             xmin=c((max(df$level)+0.3),(max(df$level)+0.3)),
                                             xmax=c(0.5,0.5),
                                             ymin=c(0,1),
                                             ymax=c(1,max(df$ci_upper, df$ci_upper_cum)))

        background_colourpanel$favours_int <- factor(background_colourpanel$favours_int,
                                                     levels = c(0,1),
                                                     labels = c("Favours control", "Favours intervention"))

        # Win ratio forest plot with overall level as separate diamond and cumulative win ratio (annotated version has axis limit adjusted to make extra space)
        wr_forest_overall_diamond_cum2 <- ggplot(df) +
                geom_rect(data=background_colourpanel, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=favours_int), alpha=0.15) +
                scale_fill_manual(values=c("#D10707", "#45E928")) +
                geom_hline(yintercept=1,color="#1A2678") +
                geom_errorbar(data=df[df$level<max(df$level), ],
                              aes(x=level-0.1, ymin=ci_lower, ymax=ci_upper, color="Level win ratio"), width=0) +
                geom_point(data=df[df$level<max(df$level), ],
                           aes(x=level-0.1, y=win_ratio, color="Level win ratio"), size=2.5, shape=4) +
                geom_line(data=df[df$level<max(df$level), ],
                          aes(x=level+0.1,y=win_ratio_cum), color="black", linetype="dashed") +
                geom_errorbar(data=df[df$level<max(df$level), ],
                              aes(x=level+0.1, ymin=ci_lower_cum, ymax=ci_upper_cum, color="Cumulative win ratio"), width=0) +
                geom_point(data=df[df$level<max(df$level), ],
                           aes(x=level+0.1, y=win_ratio_cum, color="Cumulative win ratio"), size=2.5) +
                geom_polygon(data = data.frame(x=c(max(df$level)-0.18, max(df$level),
                                                   max(df$level)+0.18, max(df$level)),
                                               y=c(df[df$level==max(df$level), "win_ratio"],
                                                   df[df$level==max(df$level), "ci_upper"],
                                                   df[df$level==max(df$level), "win_ratio"],
                                                   df[df$level==max(df$level), "ci_lower"])),
                             aes(x=x, y=y)) +
                scale_x_reverse(breaks=NULL, limits=c(max(df$level)+0.3,0.5)) +
                scale_y_continuous(limits=c(0,NA)) +
                xlab(NULL) +
                ylab(expression(bold("Win ratio"))) +
                guides(color = guide_legend(reverse = TRUE, title = NULL), fill = guide_legend(title = NULL)) +
                scale_color_manual(values=c("black", "grey40")) +
                coord_flip() +
                theme_bw()

        # Combine plots
        wr_box_overall2 + wr_forest_overall_diamond_cum2 + plot_layout(guides = 'collect')
}



# NOTE: you can't have any code like this in any file inside the R directory - it should be functions only.
# If the below code will stop R from building the package with the new function

data_test <- read.csv("test_data.csv")

wr <- winratio(id = "id", trt = "trt", active = "Treatment",
               outcomes = list(outc1 = c("death", "s", "t2death"),
                               outc2 = list(paste0("recurr", 1:9), "r", paste0("t2recurr", 1:9)),
                               outc3 = c("size", "c", ">")),
         fu = "t2death", data = df)

plot(wr)
