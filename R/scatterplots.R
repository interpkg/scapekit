

#' Signal UMAPPlot Group
#'
#' @param data dataframe
#'
#' @export
#'
Signal_UMAPPlot <- function(data=NULL, x='UMAP_1', y='UMAP_2', group='cell_type2', title='',
    decreasing_group=TRUE, color_opt='viridis', color_direc=1, color_limits=NA, 
    show_umap_lab=FALSE, xa=1.2, xb=.3, ya=1.1, yb=.25
){
    # decreasing true
    if (decreasing_group){
        data <- data[order(data[[group]], decreasing=TRUE), ]
    }
    
    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]])) + 
            geom_point(aes(color=.data[[group]]), size=0.01) +
            theme_void() +
            theme(legend.title=element_blank()) +
            theme(text=element_text(size=8)) +
            ggtitle(title)

    # color_opt: https://ggplot2.tidyverse.org/reference/scale_viridis.html
    # 'magma','inferno','plasma','viridis','cividis','rocket','mako','turbo'
    p <- p + scale_color_viridis_c(option=color_opt, direction = color_direc, limits=color_limits)

    # not used
    if (show_umap_lab){
        # customized umap
        #print(colnames(obj@reductions$umap@cell.embeddings))
        xmin <- min(data[[x]]) # UMAP-1
        xmax <- max(data[[x]])

        ymin <- min(data[[y]]) # UMAP-2
        ymax <- max(data[[y]])

        # (optional) arrow = arrow(length = unit(2, "mm"), type = "closed")
        p <- p + theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    axis.line = element_blank()) +
                # x
                annotation_custom(grob = grid::linesGrob(), xmin = xmin*xa, xmax = xmin + abs(xmin)*xb, ymin = ymin*ya, ymax = ymin*ya) +
                # y
                annotation_custom(grob = grid::linesGrob(), xmin = xmin*xa, xmax = xmin*xa, ymin = ymin*ya, ymax = ymin + abs(ymin)*yb) +
                coord_cartesian(xlim=c(xmin, xmax), ylim = c(ymin, ymax), clip = "off") +
                theme(axis.title = element_text(hjust = 0))
    }
    
    p
}






#' Scatter Plot
#'
#' @param data is data frame
#' @param group column
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#' @param point_size value 
#' @param ticks show or not
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
#'
ScatterPlotPlus <- function(
    data=NULL, 
    group='', 
    x='', 
    y='', 
    title='',
    x_lab='', 
    y_lab='', 
    point_size=0.01, 
    color_set=NULL,
    ticks=TRUE
){

    p <- ggplot(data, aes_string(x=x, y=y, color=group)) + 
        geom_point(shape = 16, size = 0.01) +
        theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(axis.ticks = element_line(linewidth = 0.3), axis.ticks.length=unit(.5, "mm")) +
        labs(title=title, x=x_lab, y=y_lab) +
        theme(plot.title = element_text(hjust = 0.5, size=6)) +
        theme(text = element_text(size = 5, face = "bold"), axis.text = element_text(size = 4))
    
    if (length(color_set) > 0){
        p <- p + scale_color_manual(values=color_set)
        #theme(legend.position="none")
    }

    if (!ticks){
        p <- p + theme(axis.text=element_blank(), axis.ticks=element_blank())
    }
    
    return(p)
}





#' Scatter Plot Highlight
#'
#' @param data is data frame
#' @param highlight value
#' @param group column
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#' @param point_size value 
#' @param ticks show or not
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
#'
ScatterPlotHighlight <- function(
    data=NULL, 
    highlight='', 
    group='', 
    x='', 
    y='', 
    x_lab='', 
    y_lab='', 
    point_size=0.01, 
    ticks=TRUE
){
    data$signal <- 'No'
    data[data[[group]]==highlight, 'signal'] <- 'Yes'

    data <- data[order(data$signal, decreasing=FALSE), ]

    p <- ggplot(data, aes_string(x=x, y=y, color='signal')) + 
        geom_point(shape = 16, size = 0.01) +
        theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(axis.ticks = element_line(linewidth = 0.3), axis.ticks.length=unit(.5, "mm")) +
        labs(title=highlight, x=x_lab, y=y_lab) +
        theme(plot.title = element_text(hjust = 0.5, size=6)) +
        theme(text = element_text(size = 5, face = "bold"), axis.text = element_text(size = 4)) +
        scale_color_manual(breaks = c("No", "Yes"), values=c("#D3D3D3", "#383b9d")) +
        theme(legend.position="none")

    if (!ticks){
        p <- p + theme(axis.text=element_blank(), axis.ticks=element_blank())
    }
    
    return(p)
}




#' Scatter Plot split
#'
#' @param data frame
#' @param group column
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#' @param point_size value 
#' @param ticks show or not
#'
#' @return plot
#'
#' @import ggplot2 dplyr
#'
#' @export
#'
#'
ScatterPlotSplit <- function(
    data=NULL, 
    group='', 
    x='', 
    y='', 
    x_lab='', 
    y_lab='', 
    color_set='#ac3282',
    point_size=0.01, 
    ticks=TRUE
){
    p <- ggplot(data, aes_string(x=x, y=y)) + 
        geom_point(data = select(data, -all_of(group)), color = "lightgray", shape = 16, size = 0.01) +
        geom_point(color=color_set, shape = 16, size = 0.01) + 
        theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(axis.ticks = element_line(linewidth = 0.3), axis.ticks.length=unit(.5, "mm")) +
        labs(x=x_lab, y=y_lab) +
        theme(plot.title = element_text(hjust = 0.5, size=6)) +
        theme(text = element_text(size = 5, face = "bold"), axis.text = element_text(size = 4)) +
        theme(legend.position="none")

    if (!ticks){
        p <- p + theme(axis.text=element_blank(), axis.ticks=element_blank())
    }

    p <- p + facet_wrap(~ .data[[group]], scales='free', drop = TRUE) +
        theme(
            strip.text = element_text(size = 6, color = "black", face = "bold"),
            strip.background = element_rect(color=NA, fill=NA))
    
    return(p)
}



 

#' Scatter Plot With Correlation
#'
#' @param data data
#' @param split_by name 
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#'
#' @return plot
#'
#' @import ggpubr ggplot2
#'
#' @export
#' 
#' @concept scatter plot with correlation and split plots by split_by
#'
ScatterPlotWithCorr <- function(data=NULL, split_by='', x='', y='', x_lab='', y_lab='')
{
    p <- ggscatter(data, x = x, y = y, 
            color='#2278B5', shape = 16, size = 0.5, alpha=0.6,
            add = "reg.line", add.params = list(color = "#E14C32", size=0.5),
            conf.int = TRUE, cor.coef = TRUE, cor.coef.size = 2.5,
            cor.coeff.args = list(method = "pearson"), ggtheme=clean_theme()
           ) + 
        theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(
           axis.ticks = element_line(linewidth = 0.3),
           axis.ticks.length=unit(1, "mm")) +
        labs(title='', x=x_lab, y=y_lab) +
        theme(plot.title = element_text(hjust = 0.5, size=10)) +
        theme(text = element_text(size = 8, face = "bold"), axis.text = element_text(size = 6)) 

    # splite by group
    if (split_by != ''){
        p <- p + facet_wrap(vars(.data[[split_by]])) +
        theme(
            strip.text = element_text(size = 8, color = "black", face = "bold"),
            strip.background = element_rect(color=NA, fill=NA)
        )
    }
    
    return(p)
}











