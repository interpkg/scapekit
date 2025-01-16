


#' Bar plot with group
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
#' @param hcols hline color
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
    x='', 
    y='', 
    title='',
    x_lab='', 
    y_lab='', 
    color_set='',
    hline='', 
    hcols='black'
){
    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]], fill=.data[[group]])) +
        geom_bar(stat = "identity", position="dodge", width = 0.75) +
        theme_classic() +
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
    if (hline != ''){
        hline <- as.numeric(stringr::str_split(hline, ',')[[1]])
        hcols <- as.numeric(stringr::str_split(hcols, ',')[[1]])

        for (i in length(hline)){
            p <- p + geom_hline(yintercept=hline[i], linetype="dashed", color = hcols[i])
        }
    }

    p
}



 


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
#' @param hcols hline color
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
    hcols='black'
){
    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]], color=.data[[group]])) +
        geom_line() + geom_point() +
        theme_classic(base_line_size=0.3) +
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
    if (hline != ''){
        hline <- as.numeric(stringr::str_split(hline, ',')[[1]])
        hcols <- as.numeric(stringr::str_split(hcols, ',')[[1]])

        for (i in length(hline)){
            p <- p + geom_hline(yintercept=hline[i], linetype="dashed", color = hcols[i])
        }
    }

    p
}











