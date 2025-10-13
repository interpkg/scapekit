
###############################
##    Scatter - Correlation
###############################

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
ScatterPlotWithCor <- function(
    data=NULL, split_by='', x='', y='', x_lab='', y_lab='', 
    dot_size=0.5, dot_col='#2278B5', alpha=0.6,
    lwd=0.5, reg_line_cor='#E14C32',
    cor_method='pearson')
{
    p <- ggscatter(data, x = x, y = y, 
            color=dot_col, shape = 16, size = dot_size, alpha=alpha,
            add = "reg.line", add.params = list(color = reg_line_cor, size=lwd),
            ggtheme=clean_theme()) + 
        stat_cor(aes(label = ..r.label..), method = cor_method, size=2) +
        theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(
           axis.ticks = element_line(linewidth = 0.3),
           axis.ticks.length=unit(0.5, "mm")) +
        labs(title='', x=x_lab, y=y_lab) +
        theme(plot.title = element_text(hjust = 0.5, size=8)) +
        theme(text = element_text(size = 6, face = "bold"), axis.text = element_text(size = 5)) 

    # splite by group
    if (split_by != ''){
        p <- p + facet_wrap(vars(.data[[split_by]])) +
        theme(
            strip.text = element_text(size = 6, color = "black", face = "bold"),
            strip.background = element_rect(color=NA, fill=NA)
        )
    }
    
    return(p)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#      Scatter - 2D
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#' Scatter2DSigSplit
#'
#' @param data data frame
#' @param group group
#' @param x axis x
#' @param y axis y
#' @param x_lab x lab
#' @param y_lab y lab
#' @param point_size point size 
#' @param split_by split by
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
Scatter2DSigSplit <- function(
    data=NULL, 
    title='',
    x='zr_score1',
    y='cc_score1',
    group='group',
    x_lab="ZR-Fusion Target Gene signal",
    y_lab="Cell-Cycling signal",
    pt_size=0.2,
    text_size=5,
    split_by='cell_type2',
    colors=c('YES'='red', 'NO'='gray'),
    hlineL=0,
    hlineH=0.2,
    vlineL=0,
    vlineH=0.2,
    lws=0.3,
    xlim=c(-0.4, 0.5)
) {
    p <- ggscatter(data, x = x, y = y, color=group, size=pt_size) + 
        labs(x=x_lab, y=y_lab) +
        theme(plot.title = element_text(hjust = 0.5, size=7)) +
        guides(color = guide_legend(override.aes = list(size = 2))) +
        theme(legend.title = element_text(size=6, face='bold')) +
        theme(text=element_text(size=text_size, face='bold', color='black')) +
        scale_colour_manual(values=colors) +
        geom_hline(yintercept=hlineL, linetype="dashed", size=lws) +
        geom_hline(yintercept=hlineH, linetype="dashed", size=lws) +
        geom_vline(xintercept=vlineL, linetype="dashed", size=lws) +      
        geom_vline(xintercept=vlineH, linetype="dashed", size=lws) +
        theme(legend.position = "none") +
        xlim(xlim)

    p <- p + facet_grid(cols = vars(.data[[split_by]])) +
        theme(strip.background = element_blank(), strip.text = element_text(size=text_size))

    return(p)
}






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#      Dot plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' Dot plot with group
#'
#' @param data frame
#' @param group column
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#' @param title name 
#' @param colors code
#' @param hline number
#' @param hcol hline color
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
DotLinePlotGroup <- function(
    data=NULL, 
    group='', 
    x='', 
    y='', 
    title='',
    x_lab='', 
    y_lab='', 
    colors='',
    hline='', 
    hcol='black'
){
    p <- ggplot(data, aes_string(x=x, y=y, group=group, color=group)) +
        geom_point() +
        geom_line() + 
        theme_classic(base_line_size=0.3) +
        labs(title=title, x=x_lab, y=y_lab) +
        theme(axis.ticks = element_line(size = 0.3)) +
        theme(plot.title = element_text(hjust = 0.5, size=8),
                text=element_text(hjust = 0.5, size=8)) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        theme(legend.title = element_blank(),
                legend.position=c(0.9, 0.8),
                legend.text = element_text(size = 6),
                legend.key.size=unit(2,"mm"))

    # set color for bar
    if (length(colors) > 1){ 
        p <- p + scale_fill_manual(values=colors)
    }

    # add hline with different color
    hline <- as.numeric(stringr::str_split(hline, ',')[[1]])
    hcol <- stringr::str_split(hcol, ',')[[1]]

    if (length(hline) > 0){
        for (i in 1:length(hline)){
            p <- p + geom_hline(yintercept=hline[i], linetype="dashed", color = hcol[i])
        }
    }

    p
}




###############################
##      Scatter - Others
###############################

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






