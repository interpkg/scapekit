#' @include calculate.R
#'
NULL


 
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




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#      Density plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

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




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#      Area plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#' Area Plot
#'
#' @param data frame
#' @param x axis name
#' @param y axis name
#' @param group column
#'
#' @return plot
#'
#' @import reshape2 ggplot2
#'
#' @export
#'
AreaPlotGroup <- function(data, x='', y='', group='', colors='', title='', angle=45)
{   
    p <- ggplot(table, aes(x=.data[[x]], y=.data[[y]], fill=.data[[group]], group=.data[[group]])) + 
        geom_area(position = 'fill') +
        theme(panel.background = element_blank(),
            axis.text.x = element_text(angle = angle, hjust=1),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title=element_blank(),
            legend.key.size = unit(3, 'mm'),
            legend.text=element_text(size=8),
            legend.position="bottom") + 
        labs(title=title, x='', y='')

    if (length(colors) > 1){ 
        p <- p + scale_fill_manual(values=colors)
    }

    p
}




#' Sing-Cell Area Plot
#'
#' @param data frame
#' @param x axis name
#' @param group column
#'
#' @return plot
#'
#' @import reshape2 ggplot2
#'
#' @export
#'
AreaPlotGroupProportion <- function(data, x='orig.ident', group='cell_type2', colors='', title='', angle=45)
{   
    # make proportion
    report <- data.frame(cbind(prop.table(table(data[[group]], data[[x]]),  margin = 2)))
    report$group <- rownames(report)

    # make new colname
    table <- reshape2::melt(report, id.vars="group", variable.name="sample", value.name="proportion")
    table$proportion <- format(round(table$proportion * 100, 2), nsmall=2)
    table$proportion <- as.numeric(as.character(table$proportion))

    p <- ggplot(table, aes(x=sample, y=proportion, fill=group, group=group)) + 
        geom_area(position = 'fill') +
        theme(panel.background = element_blank(),
            axis.text.x = element_text(angle = angle, hjust=1),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title=element_blank(),
            legend.key.size = unit(3, 'mm'),
            legend.text=element_text(size=8),
            legend.position="bottom") + 
        labs(title=title, x='', y='')

    if (length(colors) > 1){ 
        p <- p + scale_fill_manual(values=colors)
    }

    p
}








