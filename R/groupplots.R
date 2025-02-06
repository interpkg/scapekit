#' @include calculate.R
#'
NULL




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#      Bar plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

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

    p
}




#' BarPlot Two Group +- in Y-axis
#'
#' @param data frame
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#' @param title name 
#' @param split_group column
#' @param color_set code
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
#'
BarPlotGroupPosNeg <- function(
    data=NULL, 
    x='', 
    y='', 
    title='',
    x_lab='', 
    y_lab='', 
    split_group=NULL,
    reverse=FALSE,
    color_set=c('pos'='#FC766AFF', 'neg'='#5B84B1FF')
){
    data <- mutate(data, signal = ifelse(.data[[y]] > 0, 'pos', 'neg'))

    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]], fill=signal)) +
        geom_col(width=0.8) +
        theme_linedraw() +
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.spacing=unit(0.1, "lines")) +
        labs(title=title, x=x_lab, y=y_lab) +
        theme(plot.title = element_text(hjust = 0.5, size=9, face='bold'),
            text=element_text(hjust = 0.5, size=8)) +
        theme(axis.text=element_text(color='black'), 
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        theme(axis.ticks = element_line(linewidth = 0.3), axis.ticks.length=unit(0.5, "mm")) +
        theme(legend.position='none') 

    p <- p + scale_fill_manual(values=color_set)

    # reverse order 
    if (reverse){
        p <- p + scale_x_discrete(limits=rev)
    }

    if (length(split_group) > 0){
        p <- p + facet_wrap(~ .data[[split_group]]) +
            theme(strip.text=element_text(size=7, face='bold', color='black'), strip.background=element_blank())
    }

    p
}





#' BarPlot Two Group +- in X-axis
#'
#' @param data frame
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#' @param title name 
#' @param split_group column
#' @param color_set code
#'
#' @return plot
#'
#' @import ggplot2 forcats
#'
#' @export
#'
#'
BarPlotGroupPosNeg2 <- function(
    data=NULL, 
    x='', 
    y='', 
    title='',
    x_lab='', 
    y_lab='', 
    line_size=0.5,
    split_group=NULL,
    reverse=FALSE,
    color_set=c('pos'='#FC766AFF', 'neg'='#5B84B1FF')
){
    data <- mutate(data, signal = ifelse(.data[[x]] > 0, 'pos', 'neg'))

    p <- ggplot(data, aes(x=.data[[x]], y=forcats::fct_reorder(.data[[y]], .data[[x]]), fill=signal)) +
        geom_col(width=0.8) +
        theme_classic(base_line_size=line_size) +
        labs(title=title, x=x_lab, y=y_lab) +
        theme(plot.title = element_text(hjust = 0.5, size=8, face='bold'),
            text=element_text(size=8), axis.text=element_text(color='black'),
            axis.text.x=element_text(size=7)) +
        theme(axis.ticks = element_line(linewidth = line_size), axis.ticks.length=unit(1, "mm")) +
        theme(legend.position='none')
    
    p <- p + scale_fill_manual(values=color_set)

    # reverse order 
    if (reverse){
        p <- p + scale_y_discrete(limits=rev)
    }

    if (length(split_group) > 0){
        p <- p + facet_wrap(~ .data[[split_group]]) +
            theme(strip.text=element_text(size=7, face='bold', color='black'), strip.background=element_blank())
    }

    p
}





#' Bar plot with group for positive and negative
#'
#' @param data frame
#' @param split_group column
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#' @param title name 
#' @param color code
#'
#' @return plot
#'
#' @import ggplot2 stringr
#'
#' @export
#'
BarPlotSplitGroup_v1 <- function(
    data=NULL, 
    x='', 
    y='', 
    split_group='',
    title='',
    x_lab='', 
    y_lab='', 
    color=''
){  

    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]])) + 
        geom_col(width=0.8, fill=color) +
        theme_linedraw() +
        labs(title=title, x=x_lab, y=y_lab) + 
        theme(plot.title = element_text(hjust = 0.5, size=9, face = "bold")) +
        theme(text = element_text(size=7), axis.text = element_text(color='black')) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        theme(strip.text=element_text(size=7, face='bold', color='black'), strip.background=element_blank()) +
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.spacing=unit(0.1, "lines")) +
        theme(axis.ticks = element_line(linewidth = 0.3), axis.ticks.length=unit(0.5, "mm")) +
        theme(legend.title=element_blank(), legend.key.size = unit(2, 'mm'), legend.text=element_text(size=6), legend.position="bottom")

    p <- p + facet_wrap(~ .data[[split_group]]) 

    p
}





#' Bar plot with group for positive and negative II
#'
#' @param data frame
#' @param split_group column
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#' @param title name 
#' @param color code
#'
#' @return plot
#'
#' @import ggplot2 stringr
#'
#' @export
#'
BarPlotSplitGroup_v1b <- function(
    data=NULL, 
    x='', 
    y_bkg='', 
    y_tar='', 
    split_group='',
    title='',
    x_lab='', 
    y_lab='', 
    color='#c91f1f'
){  

    p <- ggplot(data) + 
        geom_bar(aes(x = .data[[x]], y = .data[[y_bkg]]), stat = "identity", fill = '#E0E0E0') +
        geom_bar(aes(x = .data[[x]], y = .data[[y_tar]]), stat = "identity", fill = color, alpha = 0.7) + 
        theme_linedraw() + 
        labs(title=title, x=x_lab, y=y_lab) + 
        theme(plot.title = element_text(hjust = 0.5, size=9, face = "bold")) +
        theme(text = element_text(size=7), axis.text = element_text(color='black')) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        theme(strip.text=element_text(size=7, face='bold', color='black'), strip.background=element_blank()) +
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.spacing=unit(0.1, "lines")) +
        theme(axis.ticks = element_line(linewidth = 0.3), axis.ticks.length=unit(0.5, "mm")) +
        theme(legend.position='none')

    p <- p + facet_wrap(~ .data[[split_group]]) 

    p
}





#' Bar plot with group for positive and negative
#'
#' @param data frame
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
BarPlotSplitGroup_v2 <- function(
    data=NULL, 
    x='', 
    y='', 
    group='',
    split_group='',
    title='',
    x_lab='', 
    y_lab='', 
    color_set=''
){  

    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]], fill=.data[[group]], group=.data[[group]])) + 
        geom_col(width=0.8) +
        theme_linedraw() + 
        labs(title=title, x=x_lab, y=y_lab) + 
        theme(plot.title = element_text(hjust = 0.5, size=9, face = "bold")) +
        theme(text = element_text(size=7), axis.text = element_text(color='black')) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        theme(strip.text=element_text(size=7, face='bold', color='black'), strip.background=element_blank()) +
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.spacing=unit(0.1, "lines")) +
        theme(axis.ticks = element_line(linewidth = 0.3), axis.ticks.length=unit(0.5, "mm")) +
        theme(legend.title=element_blank(), legend.key.size = unit(2, 'mm'), legend.text=element_text(size=6), legend.position="bottom")

    if (length(color_set) > 1){ 
        p <- p + scale_fill_manual(values=color_set)
    }

    p <- p + facet_wrap(~ .data[[split_group]]) 

    p
}






#' Bar plot for group proportion
#'
#' @param data frame
#' @param x sample name
#' @param group cell type
#' @param color_set code
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
BarPlotGroupProportion <- function(
    data, 
    x='', 
    y='ratio', 
    group='', 
    title='', 
    y_lab='Proportion (%)', 
    color_set='', 
    legend_nrow=1, 
    legend_position='bottom',
    text_size=7, 
    title_size=8, 
    angle=45, 
    line_size=0.3,
    add_label=FALSE, 
    label_color='black', 
    label_size=1.8,
    factor_x=NULL, 
    factor_group=NULL
){   
    # call proportion
    dcount <- CallProportion(data, x, group)
    # <x> <group> count total_count ratio

    # factor x
    if (length(factor_x) > 1){
        dcount[[x]] <- factor(dcount[[x]], levels=factor_x)
    }

    # factor group
    if (length(factor_group) > 1){
        dcount[[group]] <- factor(dcount[[group]], levels=factor_group)
    }

    p <- ggplot(dcount, aes(x=.data[[x]], y=.data[[y]], fill=.data[[group]], group=.data[[group]])) + 
        geom_col(width=0.8) +
        theme_classic(base_line_size=line_size) + 
        labs(title=title, x='', y=y_lab) +
        theme(plot.title = element_text(hjust = 0.5, size=title_size, face='bold')) +
        theme(text=element_text(size=text_size), 
            axis.title=element_text(size= text_size + 1), 
            axis.text=element_text(size=text_size, color='black')) +
        scale_x_discrete(guide=guide_axis(angle=angle)) +
        theme(panel.background = element_blank(),
            legend.title=element_blank(),
            legend.key.size = unit(2, 'mm'),
            legend.text=element_text(size=text_size),
            legend.position=legend_position
        ) + 
        guides(fill=guide_legend(nrow=legend_nrow, byrow=T))

    # change color
    if (length(color_set) > 1){ 
        p <- p + scale_fill_manual(values=color_set)
    }

    # add text
    if (add_label){
        p <- p + geom_text(aes(label=paste0(count,'\n','(',ratio,'%)')), position = position_stack(vjust = 0.5), color=label_color, size=label_size)
    }
    
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
AreaPlotGroup <- function(data, x='', y='', group='', color_set='', title='', angle=45)
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

    if (length(color_set) > 1){ 
        p <- p + scale_fill_manual(values=color_set)
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
AreaPlotGroupProportion <- function(data, x='orig.ident', group='cell_type2', color_set='', title='', angle=45)
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

    if (length(color_set) > 1){ 
        p <- p + scale_fill_manual(values=color_set)
    }

    p
}








