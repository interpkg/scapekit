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
#'
BarPlotGroup <- function(
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




#' BarPlot Two Group +- in Y-axis
#'
#' @param data frame
#' @param x axis name
#' @param y axis name
#' @param x_lab name
#' @param y_lab name
#' @param title name 
#' @param split_group column
#' @param colors code
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
    colors=c('pos'='#FC766AFF', 'neg'='#5B84B1FF')
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

    p <- p + scale_fill_manual(values=colors)

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
#' @param colors code
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
    colors=c('pos'='#FC766AFF', 'neg'='#5B84B1FF')
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
    
    p <- p + scale_fill_manual(values=colors)

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
#' @param colors code
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
    colors=''
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

    if (length(colors) > 1){ 
        p <- p + scale_fill_manual(values=colors)
    }

    p <- p + facet_wrap(~ .data[[split_group]]) 

    p
}






#' Bar plot for group proportion
#'
#' @param data frame
#' @param x sample name
#' @param group cell type
#' @param colors code
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
    colors='', 
    breaks = waiver(),
    legend_nrow=1, 
    legend_position='bottom',
    text_size=6, 
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
            axis.text=element_text(size=text_size, color='black'),
            axis.text.y = element_text(size = text_size - 1)
        ) +
        scale_x_discrete(guide=guide_axis(angle=angle)) +
        theme(panel.background = element_blank(),
            legend.title=element_blank(),
            legend.key.size = unit(2, 'mm'),
            legend.text=element_text(size=text_size),
            legend.position=legend_position
        ) + 
        guides(fill=guide_legend(nrow=legend_nrow, byrow=T))

    # change color
    if (length(colors) > 1){ 
        p <- p + scale_fill_manual(breaks=breaks, values=colors)
    }

    # add text
    if (add_label){
        p <- p + geom_text(aes(label=paste0(count,'\n','(',ratio,'%)')), position = position_stack(vjust = 0.5), color=label_color, size=label_size)
    }
    
    p
}






#' Bar plot for signal
#'
#' @param data frame
#'
#' @return plot
#'
#' @import ggplot2
#'
#' @export
#'
BarPlotSignal <- function(
    data, 
    x='', 
    y='ratio', 
    clusters='clusters',
    signal='group', 
    colors=c("High"="#CB4335", "Middle"="#2E86C1", "Low"="#D7DBDD"),
    breaks=c('High', 'Middle', 'Low'),
    title='', 
    x_lab='',
    y_lab='The number of cells'

){   
    # 1.order cell type
    report <- as.data.frame(table(data[[clusters]]))
    colnames(report) <- c('clusters', 'n')
    order_clusters <- (dplyr::arrange(report, desc(n)))[[clusters]]

    # 2.
    table <- data %>% count(.data[[clusters]], .data[[signal]], sort = TRUE)
    colnames(table) <- c('clusters', 'signal', 'n')
    
    #colors <- c("High"="#CB4335", "Middle"="#2E86C1", "Low"="#D7DBDD")
    p <- ggplot(table) +
            aes(x = clusters, y=n, fill=signal) +
            geom_bar(stat = "identity") + 
            coord_flip() +
            scale_x_discrete(limits = rev(order_clusters))

    p <- p + scale_fill_manual(breaks=breaks, values=colors)
    
    p <- p + theme_classic() + 
            labs(title=title, x=x_lab, y=y_lab) +
            theme(
                axis.title = element_text(size = 7),
                text = element_text(size=6),
                axis.text.y = element_text(size = 6),
                axis.title.x = element_text(size = 6),

                legend.title = element_blank(),
                legend.text = element_text(),
                legend.key.size=unit(3,"mm"),
                    
                axis.line.y = element_blank(),
                axis.line.x = element_line(color="grey"),
                axis.ticks = element_line(color="grey"),

                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                panel.border = element_blank()
            )

    return(p)
}




