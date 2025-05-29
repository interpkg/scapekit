ComplexHeatmap_Group2 <- function(
    data=NULL, 
    group='', 
    sample_id='',
    colors=c('blue', 'white', 'red'),
    zscore=FALSE,
    row_split=NULL
){

    info <- data[,c(sample_id, group)]
    col_names <- colnames(data)
    features <- col_names[!colnames(data) %in% c(sample_id, group)]
    d_mtx <- as.matrix(t(data[, features]))

    if (zscore){
        d_mtx = t(scale(t(d_mtx)))
        colors = circlize::colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
        #colors = circlize::colorRamp2(c(-2, -1, 0, 1, 2), c("#09103b", "#5f79cf", "white", "#eb6565", "#540506"))
        ht_title = "Row Z-Score"
    }

    ha <- HeatmapAnnotation(
                Group = info[[group]],
                Sample = info[[sample_id]],
                annotation_name_gp = gpar(fontsize = 7)
            )

    ht_exp <- Heatmap(
                d_mtx,

                col = colors,
                    
                show_row_names = T,
                row_names_side = "left",
                row_names_gp = gpar(fontsize = 7),
                show_row_dend = F,
                cluster_rows = T,

                show_column_names = F,
                column_names_rot = 60,
                column_names_gp = gpar(fontsize = 7),
                show_column_dend = T,
                cluster_columns = T,

                # split row
                row_split = row_split,
                row_gap = unit(0.3, "mm"),
                row_title_gp = grid::gpar(fontsize = 7),
                
                # split column
                column_split = factor(info[[group]], levels = unique(info[[group]])),
                cluster_column_slices = FALSE,
                column_gap = unit(0.3, "mm"),
                column_title_gp = grid::gpar(fontsize = 7),
                
                top_annotation = ha,

                # legend
                heatmap_legend_param = list(
                        title_gp = gpar(fontsize = 7), 
                        labels_gp = gpar(fontsize = 6),
                        legend_width = unit(6, "cm")
                    )
            )

    return(ht_exp)
}