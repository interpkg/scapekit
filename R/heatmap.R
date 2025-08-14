
#' Correct data for heatmap
CorrectInfoByMarkerData <- function(
    diff_marker=NULL,
    mtx=NULL,
    meta=NULL
){
    library(dplyr)
    #1.markers
    diff_marker <- diff_marker %>% group_by(gene) %>% filter(n() == 1) %>% ungroup()
    rownames(diff_marker) <- 1:nrow(diff_marker)
    diff_marker$index <- 1:nrow(diff_marker)
    
    #2.exp matrix
    marker_genes <- diff_marker$gene
    mtx <- mtx[marker_genes, ]

    #3.meta
    meta <- meta[colnames(mtx), ]

    return(list(diff_marker, mtx, meta))
}



#' HeatMap_ExpGroups_Vertical
#'
#' @param data dataframe
#' @param meta top annotation group-1
#' @param diff_marker top annotation group-2
#' @param group group name
#' @param sort_group sorted group name
#' @param col_group color group
#' @param show_gene show genes
#' @param scaled scaled or not
#'
#' @export
#'
Heatmap_DiffMarkers_Vertical <- function(
    data=NULL,
    meta=NULL,
    diff_marker=NULL,
    group='cluster',
    sort_group=NULL,
    col_group=NULL, 
    show_gene=NULL,
    scaled=TRUE
) { 

    data_list <- CorrectInfoByMarkerData(diff_marker=diff_marker, mtx=data, meta=meta)
    diff_marker <- data_list[[1]]
    mtx <- data_list[[2]]
    meta <- data_list[[3]]
    

    # z-score: row z-score
    if (scaled){ mtx <- t(scale(t(mtx))) }

    set.seed(42)
    
    top_anno <- HeatmapAnnotation(
                Group = anno_simple(x = meta[[group]], simple_anno_size = unit(2, "mm"), col=col_group),
                annotation_name_side = "right",
                annotation_name_gp = gpar(fontsize = 6)
        )

    

    gene_at <- diff_marker$index[diff_marker$gene %in% show_gene]
    gene_anno <- rowAnnotation( link = anno_mark(at=gene_at, labels=show_gene, which="bottom", link_width=unit(2,"mm"), labels_gp=gpar(fontsize = 5)) )

    col_zscore = colorRamp2(c(-2, 0, 2), c("#2E86C1", "white", "#CB4335"))
    title_ht = "Row Z-Score"

    ht <- Heatmap(
                as.matrix(mtx),
                    
                col = col_zscore,
                    
                show_row_names = F,
                show_row_dend = F,
                cluster_rows = T,

                show_column_names = F,
                show_column_dend = F,
                cluster_columns = T,

                # split
                #column_title = NULL,
                column_title_gp = gpar(fontsize = 7),
                column_split = factor(meta[[group]], levels=sort_group), 
                cluster_column_slices = F,
                column_gap = unit(0.5, "mm"),
                
                
                # split rows to three groups
                row_title = 'Differentially expressed genes', 
                row_title_gp = gpar(fontsize = 7),
                row_split = factor(diff_marker$cluster, levels=sort_group),
                cluster_row_slices = F,
                row_gap = unit(0.5, "mm"),

                # top-anno
                top_annotation = top_anno,
                right_annotation = gene_anno,

                # legend
                heatmap_legend_param = list(
                        title = 'Expression\nZ-score',
                        title_gp = gpar(fontsize = 5), 
                        labels_gp = gpar(fontsize = 5),
                        grid_width = unit(2, "mm"),
                        grid_height = unit(2, "mm")
                        )
            )

    return(ht)
}







