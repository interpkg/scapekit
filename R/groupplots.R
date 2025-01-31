
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#      Bar plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' Bar plot count
#'
#' @param data frame
#' @param x axis name
#' @param y_lab name
#' @param color color code
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
BarPlotCount <- function(
    data=NULL, 
    x='', 
    y_lab='', 
    color='#681989'
){
    df <- as.data.frame(table(data[[x]]))
    colnames(df) <- c(x, 'n_count')
    
    # barplot
    max_n <- max(df$n_count) * 1.2
    p <- ggplot(df, aes(x=.data[[x]], y=n_count)) +
        geom_bar(fill = color, stat = "identity") +
        geom_text(aes(label = n_count), vjust = 0.5, hjust = -0.1, size = 3) + 
        theme_classic() + 
        aes(x=reorder(.data[[x]], n_count, sum), y=n_count) + 
        coord_flip() + 
        ylim(0, max_n) +
        labs(title='', x="", y=y_lab)

    p
}




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
BarPlotSplitGroup <- function(
    data=NULL, 
    x='', 
    y='', 
    split_group='',
    title='',
    x_lab='', 
    y_lab='', 
    color='blue'
){  

    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]])) + 
        geom_col(fill=color) + 
        theme_bw() +
        labs(x=x_lab, y=y_lab) + 
        theme(legend.position='none') + 
        theme(text = element_text(size=7), axis.text = element_text(color='black')) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    p <- p + facet_wrap(~ .data[[split_group]]) 

    p
}




#' Bar plot with group for positive and negative v2
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
BarPlotSplitGroup_v2 <- function(
    data=NULL, 
    x='', 
    y_all='', 
    y_tar='', 
    split_group='',
    title='',
    x_lab='', 
    y_lab='', 
    color='#c91f1f'
){  

    p <- ggplot(data) + 
        geom_bar(aes(x = .data[[x]], y = .data[[y_all]]), stat = "identity", fill = '#E0E0E0') +
        geom_bar(aes(x = .data[[x]], y = .data[[y_tar]]), stat = "identity", fill = color, alpha = 0.7) + 
        theme_linedraw()+ labs(x='', y='') +
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




#' Bar plot for group proportion
#'
#' @param data frame
#' @param x sample name
#' @param group cell type
#' @param color_set code
#'
#' @return plot
#'
#' @import ggplot2 randomcoloR
#'
#' @export
#'
BarPlotGroupProportion <- function(data, x='', group='', color_set='')
{
    x_size <- length(unique(data[[x]]))

    my_cols <- c()
    if (color_set == ''){
        set.seed(1234)                                
        color_set <- randomcoloR::distinctColorPalette(x_size)
    }

    dcount <- data %>% group_by(.data[[x]], .data[[group]]) %>% count()
    colnames(dcount) <- c(x, group, 'n')
    n <- data.frame(table(data[[x]]))
    dcount$n_total <- n$Freq[match(dcount[[x]], n$Var1)]
    dcount$ratio <- round(dcount$n/dcount$n_total * 100, 1)

    p <- ggplot(dcount, aes(x=.data[[x]], y=ratio, fill=.data[[group]], group=.data[[group]])) + 
        geom_col(width=0.7) +
        theme_classic() + 
        theme(text=element_text(size=6), axis.text=element_text(size=6)) +
        scale_x_discrete(guide=guide_axis(angle=45)) +
        theme(panel.background = element_blank(),
            legend.title=element_blank(),
            legend.key.size = unit(2, 'mm'),
            legend.text=element_text(size=6),
            legend.position="bottom"
        ) + 
        guides(fill=guide_legend(nrow=2, byrow=T)) +
        labs(x='', y='Proportion')

    p <- p + scale_fill_manual(values=color_set)

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
AreaPlot <- function(data, x='', y='', group='', color_set='', title='', angle=45)
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
AreaPlotProportion <- function(data, x='Sample', group='cell_type2', color_set='', title='', angle=45)
{   
    table <- reshape2::melt(data, id.vars=.data[[group]], variable.name=.data[[x]], value.name="proportion")
    table$proportion <- format(round(table$proportion * 100, 2), nsmall=2)
    table$proportion <- as.numeric(as.character(table$proportion))

    p <- ggplot(table, aes(x=.data[[x]], y=proportion, fill=.data[[group]], group=.data[[group]])) + 
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








