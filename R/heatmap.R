
##########################################
##         Diff Markers
##########################################

#' Correct data for heatmap
CorrectInfoByMarkerData <- function(
    mtx=NULL,
    meta=NULL,
    diff_marker=NULL
){
    library(dplyr)
    #1.markers
    diff_marker <- as.data.frame(diff_marker) %>% group_by(gene) %>% filter(n() == 1) %>% ungroup()
    rownames(diff_marker) <- 1:nrow(diff_marker)
    diff_marker$index <- 1:nrow(diff_marker)
    
    #2.exp matrix
    marker_genes <- diff_marker$gene
    mtx <- as.data.frame(mtx)[marker_genes, ]

    #3.meta
    meta <- as.data.frame(meta)[colnames(mtx), ]

    return(list(mtx, meta, diff_marker))
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
Heatmap_DiffMarkers_Vert <- function(
    data=NULL,
    meta=NULL,
    diff_marker=NULL,
    group='cluster',
    sort_group=NULL,
    col_group=NULL, 
    show_gene=NULL,
    scaled=TRUE,
    gap=0.4
) { 

    data_list <- CorrectInfoByMarkerData(mtx=data, meta=meta, diff_marker=diff_marker)
    mtx <- data_list[[1]]
    meta <- data_list[[2]]
    diff_marker <- data_list[[3]]
    print('After Correction:')
    print(paste0('Matrix: ', dim(mtx)))
    print(paste0('Meta: ', dim(meta)))
    print(paste0('Markers: ', dim(diff_marker)))

    # z-score: row z-score
    if (scaled){ mtx <- t(scale(t(mtx))) }

    set.seed(42)
    
    haT <- HeatmapAnnotation(
                Group = anno_simple(x = meta[[group]], simple_anno_size = unit(2, "mm"), col=col_group),
                annotation_name_side = "right",
                annotation_name_gp = gpar(fontsize = 6)
        )

    gene_at <- diff_marker$index[diff_marker$gene %in% show_gene]
    haR <- rowAnnotation( link = anno_mark(at=gene_at, labels=show_gene, which="bottom", link_width=unit(2,"mm"), labels_gp=gpar(fontsize = 5)) )

    col_score = circlize::colorRamp2(c(-2, 0, 2), c("#2E86C1", "white", "#CB4335"))
    title_ht = "Row Z-Score"

    ht <- Heatmap(
                as.matrix(mtx),
                    
                col = col_score,
                    
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
                column_gap = unit(gap, "mm"),
                
                
                # split rows to three groups
                row_title = 'Differentially expressed genes', 
                row_title_gp = gpar(fontsize = 7),
                row_split = factor(diff_marker$cluster, levels=sort_group),
                cluster_row_slices = F,
                row_gap = unit(gap, "mm"),

                # top-anno
                top_annotation = haT,
                right_annotation = haR,

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





##########################################
##         Motif
##########################################

#' ComplexHeatmap Motif-TF Group-2x
#'
#' @param data dataframe
#'
#' @export
#'
Heatmap_Motif_Group2 <- function(
    data=NULL, 
    meta=NULL,
    group='', 
    sort_group=NULL,
    sample_id='',
    diff_marker=NULL,
    show_gene=NULL,
    ht_title ='Motif Score', 
    col_group=NULL, 
    col_sample=NULL,
    gap=0.4,
    border=FALSE
){

    data_list <- CorrectInfoByMarkerData(mtx=data, meta=meta, diff_marker=diff_marker)
    mtx <- data_list[[1]]
    meta <- data_list[[2]]
    diff_marker <- data_list[[3]]

    # z-score: row z-score
    if (scaled){ mtx <- t(scale(t(mtx))) }


    haT <- HeatmapAnnotation(
                Group = anno_simple(x = meta[[group]], simple_anno_size = unit(2, "mm"), col=col_group),
                Sample = anno_simple(x = meta[[sample_id]], simple_anno_size = unit(2, "mm"), col=col_sample),
                annotation_name_side = "right",
                annotation_name_gp = gpar(fontsize = 6, fontface="bold")
        )

    gene_at <- diff_marker$index[diff_marker$gene %in% show_gene]
    haR <- rowAnnotation( link = anno_mark(at=gene_at, labels=show_gene, which="bottom", link_width=unit(2,"mm"), labels_gp=gpar(fontsize = 5), padding = unit(1, "mm")) )



    col_score <- circlize::colorRamp2(c(-2, -1, 0, 1, 2), c("#440154FF", "#414487FF", "#2A788EFF", "#7AD151FF", "#FDE725FF"))

    ht <- Heatmap(
                as.matrix(mtx),
                
                col = col_score,

                show_row_names = F,
                row_names_gp = gpar(fontsize = 6),
                show_row_dend = F,
                cluster_rows = F,

                show_column_names = F,
                column_names_rot = 60,
                column_names_gp = gpar(fontsize = 6, fontface="bold"),
                show_column_dend = T,
                cluster_columns = T,

                # split row
                row_split = factor(diff_marker$cluster, levels=sort_group),
                row_gap = unit(gap, "mm"),
                row_title = NULL,
                cluster_row_slices = FALSE,
                #row_title_gp = grid::gpar(fontsize = 7),
                
                # split column
                column_split = factor(meta[[group]], levels=sort_group),
                cluster_column_slices = FALSE,
                column_gap = unit(gap, "mm"),
                column_title_gp = grid::gpar(fontsize = 8, fontface="bold"),

                border = border,
                
                top_annotation = haT,
                right_annotation = haR,

                # legend
                heatmap_legend_param = list(
                        title = ht_title,
                        direction = "horizontal",
                        title_position = "lefttop",
                        title_gp = gpar(fontsize = 8, fontface="bold"), 
                        labels_gp = gpar(fontsize = 6),
                        legend_width = unit(4, "cm"),
                        grid_height = unit(3, "mm")
                    )
            )

    lgd1 = Legend(title = "Group", labels = names(col_group), legend_gp = gpar(fill = col_group, fontsize = 6), nrow=1)
    lgd2 = Legend(title = "Sample", labels = names(col_sample), legend_gp = gpar(fill = col_sample, fontsize = 6), nrow=1)
    pd = packLegend(list = list(lgd1, lgd2), direction = "horizontal", max_width = unit(10, "cm"), column_gap = unit(5, "mm"), row_gap = unit(5, "mm"))

    #-- draw plot
    draw(ht, heatmap_legend_side = "bottom", annotation_legend_side = "bottom", annotation_legend_list = pd)

}




