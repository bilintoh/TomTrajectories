StackedTrajectories <- function(Trajectories,
                                Category = "CATEGORY",
                                xAxisText = 16,
                                yAxisText = 16,
                                axisTitle = 20,
                                legendText = 20,
                                plotTitle = 18) {
  options(warn=-1)

  ggplot() +

    # Create stacked bars for gain and loss
    geom_bar(data = as.data.frame(Trajectories$NonetData),
             aes(fromToTimepoints,value, fill = variable ,width = X2 * 2),
             stat = "identity",
             position = "identity",
             linetype ="solid",
             color = "white",
             size = 0.2) +

    scale_fill_manual(values = c(  "#000080","#8b0000"),
                      guide = guide_legend(title = " ")) +
    new_scale_fill() +

    # Create stacked bars for net
    geom_bar(data = as.data.frame(Trajectories$NetData),
             aes(fromToTimepoints,
                 value,
                 fill = variable,
                 width = X2 * 2),
             stat = "identity",
             position = "identity",
             linetype ="solid",
             color = "limegreen",
             size = 1.2)+

    scale_fill_manual(values = c(  "#000080","#8b0000"),
                      guide = guide_legend(title = " ")) +

    geom_line(data = data.frame(x = as.numeric(as.character(Trajectories$NonetData$fromToTimepoints)),
                                Rate = Trajectories$GrossLoss,
                                avg ="gross"),
              aes(x=0,y=Rate,linetype=" gross ",group = 1),
              colour = "snow4",
              size = 1.25)+

    # Plot Gross gain and loss line
    geom_abline(slope=0,
                intercept=Trajectories$GrossGain,
                col = "snow4",
                size = 1.3)+

    geom_abline(slope = 0,
                intercept=Trajectories$GrossLoss,
                col = "snow4",
                size = 1.3)+

    facet_grid(. ~fromToTimepoints,
               scales = "free_x",
               space = "free_x")+

    theme(panel.spacing.x = unit(0, "lines"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          text = element_text(size = 8)) +

    theme(panel.background = element_rect(fill = "transparent",
                                          colour = NA))+

    scale_x_discrete(expand = c(0, 0))+

    geom_abline(slope = 0,
                intercept = 0,
                col = "white",
                size = 0.8) +

    xlab("Time Interval") +

    ylab(Trajectories$yaxisLable)+

    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.border = element_rect(color = "white",
                                  fill = NA,
                                  size = NA))+

    theme(axis.text.y = element_text(size = yAxisText,face="bold"))+

    theme(axis.text.y = element_text(face="bold")) +

    theme(axis.line.y = element_line(color = "black",
                                     size = 1)) +

    theme(axis.line.x = element_line(color = "black",
                                     size = 1)) +

    ggtitle(paste("CHANGE INVOLVING", Category, "DURING",
                  Trajectories$numbTimeIntervals,"TIME INETRVALS FROM"
                  ,Trajectories$lastTimeInterval ))+

    theme(plot.title = element_text(hjust = 0.5,
                                    size = plotTitle,
                                    face = "bold"))+

    theme(axis.text=element_text(size = xAxisText,face="bold"),
          axis.title=element_text(size = axisTitle,face="bold"),
          legend.position= 'bottom',
          legend.text = element_text( size = legendText,
                                      face = "bold"))+
    theme(legend.position = "right")+
    theme(legend.title = element_blank(),
          legend.key = element_rect(colour = NA,
                                    fill = NA) )


}


