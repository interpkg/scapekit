
#' Scatter Plot
#'
#' @param data frame
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
#'
ScatterPlotHighlight <- function(df=NULL, highlight='', split_by='', x='', y='', x_lab='', y_lab='')
{
    df <- df[order(df[highlight], decreasing=TRUE), ]

    p <- ggplot(df, aes_string(x=x, y=y, color=highlight)) + 
        geom_point(shape = 16, size = 0.5, alpha=0.6) +
        theme_linedraw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(axis.ticks = element_line(linewidth = 0.3), axis.ticks.length=unit(1, "mm")) +
        labs(x=x_lab, y=y_lab) +
        theme(plot.title = element_text(hjust = 0.5, size=10)) +
        theme(text = element_text(size = 8, face = "bold"), axis.text = element_text(size = 6)) +
        scale_color_manual(breaks = c("No", "Yes"), values=c("#D3D3D3", "#383b9d")) +
        theme(legend.position="none")

    # splite by group
    if (split_by != ''){
        p <- p + facet_wrap(vars(split_by)) +
        theme(
            strip.text = element_text(size = 8, color = "black", face = "bold"),
            strip.background = element_rect(color=NA, fill=NA)
        )
    }
    

    return(p)
}


 


#' Scatter Plot With Correlation
#'
#' @param data frame
#'
#' @return plot
#'
#' @import ggpubr ggplot2
#'
#' @export
#' 
#' @concept scatter plot with correlation and split plots by split_by
#'
ScatterPlotWithCorr <- function(df=NULL, split_by='', x='', y='', x_lab='', y_lab='')
{
    p <- ggscatter(df, x = x, y = y, 
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
        p <- p + facet_wrap(vars(split_by)) +
        theme(
            strip.text = element_text(size = 8, color = "black", face = "bold"),
            strip.background = element_rect(color=NA, fill=NA)
        )
    }
    

    return(p)
}








