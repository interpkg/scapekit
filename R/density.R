


###############################
##      Density plot
###############################

#' Density Ridges Gradient Split Group
#'
#' @param data frame
#' @param x1 axis name
#' @param y axis name
#' @param group column
#'
#' @return plot
#'
#' @import ggplot2 ggridges
#'
#' @export
#'
DensityRidgesGradient_SplitGroup <- function(data, x1='', y='', split_group='', n_row=1)
{
    p <- ggplot(data, aes(x = .data[[x1]], y = .data[[y]], fill = stat(x))) +
          geom_density_ridges_gradient(lwd = 0.1, scale = 1, gradient_lwd = 1.) +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25))) +
          scale_fill_viridis_c(name = "Pseudotime", option = "C") +
          theme_linedraw()+ labs(x='', y='') +
          theme(strip.text=element_text(size=7, face='bold', color='black'), strip.background=element_blank()) +
          theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.spacing=unit(0.1, "lines")) +
          theme(axis.text=element_text(size=6)) +
          theme(legend.position='bottom',
                legend.title=element_text(size=6),
                legend.text=element_text(size=6),
                legend.key.height=unit(0.4,"line"),
                legend.key.size = unit(0.8, 'lines')) +
          theme(axis.ticks = element_line(linewidth = 0.3),
                axis.ticks.length=unit(0.5, "mm")) +
          geom_vline(xintercept = 5, linetype="dashed", color = "#696969", size=0.1) +
          geom_vline(xintercept = 15, linetype="dashed", color = "#696969", size=0.1)

    p <- p + facet_wrap(~ .data[[split_group]], nrow=n_row)

    p
}





###############################
##    Density - Pseudotime
###############################

#' Density plot for pseudotime
#'
#' @param data frame
#' @param x axis name
#' @param y axis name
#' @param t1 early time point
#' @param t2 later time point
#'
#' @return plot
#'
#' @import ggplot2 ggridges ggpubr
#'
#' @export
#'
DensityPlot_Pseudotime <- function(
    data, 
    x='pseudotime', 
    y='cell_type2', 
    t1=NULL, 
    t2=NULL)
{ 

    p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = after_stat(x))) +
        geom_density_ridges_gradient(scale = 1, gradient_lwd = 1) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25))) +
        scale_fill_viridis_c(name = "Pseudotime", option = "C") +
        theme_ridges(font_size = 6, grid = FALSE) + 
        theme(axis.title = element_blank()) +
        theme(axis.text=element_text(size=6)) +
        theme(legend.position='bottom',
                legend.text=element_text(size=5),
                legend.key.height=unit(0.4,"line"),
                legend.key.size = unit(0.8, 'lines')) +
        theme(axis.ticks = element_line(linewidth = 0.1), axis.ticks.length=unit(1, "mm"))

    if (length(t1)>0){
        p <- p + geom_vline(xintercept = t1, linetype="dashed", color = "#696969", size=0.3)
    }
    if (length(t2)>0){
        p <- p + geom_vline(xintercept = t2, linetype="dashed", color = "#696969", size=0.3)
    }

    p
}




