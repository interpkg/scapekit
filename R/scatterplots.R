
###############################
##    Scatter - UMAP
###############################


#' Signal UMAPPlot Group
#'
#' @param data dataframe
#'
#' @export
#'
UMAPSignalSplit <- function(data=NULL, 
                x='UMAP_1', y='UMAP_2', 
                sample='Sample', signal='signal', group='group',
                sorted_sample='',
                decreasing_group=FALSE,
                pt_size=0.01,
                title='',
                legend_title='Signal',
                legend_size=1,
                colors=c("High"="#CB4335", "Medium"="#2E86C1", "Low"="#D7DBDD"),
                breaks=c('High', 'Medium', 'Low'),
                ncol=6
){
    # decreasing or not by signal
    data <- data[order(data[[signal]], decreasing=decreasing_group), ]

    if (length(sorted_sample) > 2){
        data[[sample]] <- factor(data[[sample]], levels=sorted_sample)
    }
    
    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]])) + 
            geom_point(aes(color=.data[[group]]), size=pt_size) +
            theme_classic(base_line_size=0.1) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
            theme(axis.ticks = element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) +
            labs(title=title, x=x, y=y, color=group) +
            theme(plot.title = element_text(hjust = 0.5, size=8)) +
            theme(text = element_text(size = 6, face = "bold"))

    p <- p + scale_color_manual(name = legend_title, breaks=breaks, values=colors) +
            theme(legend.title=element_text(size=5, face = "bold")) +
            guides(color=guide_legend(override.aes=list(size=legend_size)))

    p <- p + facet_wrap(~ .data[[sample]], ncol=ncol) +
        theme(strip.background = element_blank(), strip.text = element_text(size = 5.5, color = "black", face = "bold")) +
        theme(panel.spacing=unit(1, 'mm', data=NULL))
    
    return(p)
}





#' UMAPSignal
#'
#' @param data dataframe
#'
#' @export
#'
UMAPSignal <- function(
    data=NULL, 
    x='UMAP_1', y='UMAP_2', signal='pseudotime', 
    title='',
    order_dec=FALSE, 
    point_size=0.01, 
    color='rocket', color_direc=-1,
    show_umap_lab=FALSE
){
    # decreasing true
    data <- data[order(data[[signal]], decreasing=order_dec), ]
    
    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]])) + 
            geom_point(aes(color=.data[[signal]]), size=point_size) +
            theme_classic(base_line_size=0.1) +
            theme(legend.title=element_blank(),
                legend.key.width = unit(3, 'mm'),
                legend.key.height = unit(4, 'mm')) +
            theme(text=element_text(size=6)) +
            ggtitle(title) +
            theme(plot.title = element_text(hjust = 0.5, size=8, face = "bold"))

    # color_opt: https://ggplot2.tidyverse.org/reference/scale_viridis.html
    # 'magma','inferno','plasma','viridis','cividis','rocket','mako','turbo'
    p <- p + scale_color_viridis_c(option=color, direction = color_direc, na.value='#E0E0E0')
    
    p
}



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



 






##--------------------------------------------------- // Not sure
#' Signal UMAPPlot Group
#'
#' @param data dataframe
#'
#' @export
#'
Signal_UMAPPlot <- function(data=NULL, x='UMAP_1', y='UMAP_2', group='cell_type2', title='',
    decreasing_group=FALSE, 
    point_size=0.01, color='rocket', color_direc=-1, color_limits=c(0.1, 3), 
    show_umap_lab=FALSE, 
    xa=1.1, xb=.3, ya=1.05, yb=.25,
    x_hjust=.03, y_hjust=.04
){
    # decreasing true
    data <- data[order(data[[group]], decreasing=decreasing_group), ]

    # change data based on color limits
    if (length(color_limits) > 0){
        limits_max_val <- color_limits[2]
        data[[group]][data[[group]] > limits_max_val] = limits_max_val
        print(max(data[[group]]))
    }
    
    p <- ggplot(data, aes(x=.data[[x]], y=.data[[y]])) + 
            geom_point(aes(color=.data[[group]]), size=point_size) +
            theme_void() +
            theme(legend.title=element_blank(),
                legend.key.width = unit(3, 'mm'),
                legend.key.height = unit(4, 'mm')) +
            theme(text=element_text(size=8)) +
            ggtitle(title) +
            theme(plot.title = element_text(hjust = 0.5, size=8, face = "bold"))

    # color_opt: https://ggplot2.tidyverse.org/reference/scale_viridis.html
    # 'magma','inferno','plasma','viridis','cividis','rocket','mako','turbo'
    p <- p + scale_color_viridis_c(option=color, direction = color_direc, limits=color_limits, na.value='#E0E0E0')

    # not used
    if (show_umap_lab){
        # customized umap
        #print(colnames(obj@reductions$umap@cell.embeddings))
        xmin <- min(data[[x]]) # UMAP-1
        xmax <- max(data[[x]])

        ymin <- min(data[[y]]) # UMAP-2
        ymax <- max(data[[y]])

        # (optional) arrow = arrow(length = unit(2, "mm"), type = "closed")
        p <- p + 
                # x
                annotation_custom(grob = grid::linesGrob(), xmin = xmin*xa, xmax = xmin + abs(xmin)*xb, ymin = ymin*ya, ymax = ymin*ya) +
                # y
                annotation_custom(grob = grid::linesGrob(), xmin = xmin*xa, xmax = xmin*xa, ymin = ymin*ya, ymax = ymin + abs(ymin)*yb) +
                coord_cartesian(xlim=c(xmin, xmax), ylim = c(ymin, ymax), clip = "off") +
                theme(axis.title.x = element_text(hjust = x_hjust), axis.title.y = element_text(angle=90, hjust = y_hjust))
    }
    
    p
}





#' Signal UMAPPlot Split
#'
#' @param data dataframe
#'
#' @export
#'
Signal_UMAPPlotSplit <- function(data=NULL, x='UMAP_1', y='UMAP_2', 
    group='cell_type2', split_by='orig.ident',
    title='', decreasing_group=FALSE, 
    point_size=0.01, color='rocket', color_direc=-1, color_limits=c(0.1, 3), 
    show_umap_lab=FALSE, 
    xa=1.1, xb=.3, ya=1.05, yb=.25,
    x_hjust=.03, y_hjust=.04
){
    # decreasing true
    data <- data[order(data[[group]], decreasing=decreasing_group), ]
    
    # change data based on color limits
    if (length(color_limits) > 0){
        limits_max_val <- color_limits[2]
        data[[group]][data[[group]] > limits_max_val] = limits_max_val
        print(max(data[[group]]))
    }
    
    # Create a background dataset WITHOUT the faceting variable (split_by)
    d_background <- data
    d_background[[split_by]] <- NULL  # Remove the split_by column

    # Build the plot
    p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
      # Plot ALL data as background (no split_by column → appears in all facets)
      geom_point(
        data = d_background,  # Use background data without split_by
        color = "#E0E0E0", 
        alpha = 0.3, 
        shape = 16, 
        size = point_size
      ) +
      # Plot main data (colored by group) WITH faceting
      geom_point(
        aes(color =.data[[group]]),
        shape = 16,
        size = point_size
      ) +
      # Facet by split_by (only affects the main data layer)
      facet_wrap(vars(.data[[split_by]])) +
      # Use discrete color scale (since group is converted to a factor)
      scale_color_viridis_c(
        option = color,
        direction = color_direc,
        na.value = '#E0E0E0'
      ) +
      # Theme and labels
      theme_void() +
      theme(
        legend.title = element_blank(),
        legend.key.width = unit(3, 'mm'),
        legend.key.height = unit(4, 'mm'),
        text = element_text(size = 8),
        strip.text = element_text(size = 8, color = "black", face = "bold"),
        strip.background = element_rect(color = NA, fill = NA),
        plot.title = element_text(hjust = 0.5, size = 8, face = "bold")
      ) +
      ggtitle(title)


    if (show_umap_lab){
        # customized umap
        #print(colnames(obj@reductions$umap@cell.embeddings))
        xmin <- min(data[[x]]) # UMAP-1
        xmax <- max(data[[x]])

        ymin <- min(data[[y]]) # UMAP-2
        ymax <- max(data[[y]])

        # (optional) arrow = arrow(length = unit(2, "mm"), type = "closed")
        p <- p + 
                # x
                annotation_custom(grob = grid::linesGrob(), xmin = xmin*xa, xmax = xmin + abs(xmin)*xb, ymin = ymin*ya, ymax = ymin*ya) +
                # y
                annotation_custom(grob = grid::linesGrob(), xmin = xmin*xa, xmax = xmin*xa, ymin = ymin*ya, ymax = ymin + abs(ymin)*yb) +
                coord_cartesian(xlim=c(xmin, xmax), ylim = c(ymin, ymax), clip = "off") +
                theme(axis.title.x = element_text(hjust = x_hjust), axis.title.y = element_text(angle=90, hjust = y_hjust))
    }
    

    return(p)

}


##--------------------------------------------------- \\ 










