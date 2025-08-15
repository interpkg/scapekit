#' @include calculate.R
#'
NULL


 


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








