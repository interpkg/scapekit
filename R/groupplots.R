
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#      Bar plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' Bar plot with group
#'
#' @param data frame
#' @param group column
#' @param split_group splite group
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#' @param title name 
#' @param color_set code
#' @param hline number
#' @param hcol hline color
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
#'
BarPlotGroup <- function(
    data=NULL, 
    group='', 
    split_group='',
    x='', 
    y='', 
    title='',
    x_lab='', 
    y_lab='', 
    color_set='',
    hline='', 
    hcol='black'
){
    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]], fill=.data[[group]])) +
        geom_bar(stat = "identity", position="dodge", width = 0.75) +
        theme_classic(base_line_size=0.3) +
        labs(title=title, x=x_lab, y=y_lab) +
        theme(plot.title = element_text(hjust = 0.5, size=8),
                text=element_text(hjust = 0.5, size=8)) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        theme(legend.title = element_blank(),
                legend.position=c(0.9, 0.8),
                legend.text = element_text(size = 6),
                legend.key.size=unit(2,"mm"))

    # set color for bar
    if (color_set != ''){
        color_set <- as.numeric(stringr::str_split(color_set, ',')[[1]])
        p <- p + scale_fill_manual(values=color_set)
    }

    # add hline with different color
    hline <- as.numeric(stringr::str_split(hline, ',')[[1]])
    hcol <- stringr::str_split(hcol, ',')[[1]]

    if (length(hline) > 0){
        for (i in 1:length(hline)){
            p <- p + geom_hline(yintercept=hline[i], linetype="dashed", color = hcol[i])
        }
    }

    # split group
    if (split_group != ''){
        p <- p + facet_wrap(~ .data[[split_group]]) + theme(legend.position='none')
    }

    p
}





#' Bar plot with group for positive and negative
#'
#' @param data frame
#' @param group column
#' @param split_group column
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#' @param title name 
#' @param color_set code
#'
#' @return plot
#'
#' @import ggplot2 stringr
#'
#' @export
#'
BarPlotGroupPosNeg <- function(
    data=NULL, 
    x='', 
    y='', 
    group='', 
    split_group='',
    title='',
    x_lab='', 
    y_lab='', 
    color_set='blue'
){  
    color_set <- stringr::str_split(color_set,',')[[1]]

    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]], fill=.data[[group]])) + 
        geom_col() + 
        theme_bw() +
        labs(x=x_lab, y=y_lab) + 
        scale_fill_manual(values=color_set) +
        theme(legend.position='none') + theme(text = element_text(size=7), axis.text = element_text(color='black')) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    p <- p + facet_wrap(~ .data[[split_group]]) 

    p
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
#' @param color_set code
#' @param hline number
#' @param hcol hline color
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
#'
DotLinePlotGroup <- function(
    data=NULL, 
    group='', 
    x='', 
    y='', 
    title='',
    x_lab='', 
    y_lab='', 
    color_set='',
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
    if (color_set != ''){
        color_set <- as.numeric(stringr::str_split(color_set, ',')[[1]])
        p <- p + scale_color_manual(values=color_set)
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











