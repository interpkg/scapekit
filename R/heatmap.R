

#' Customized Legend
#'
#' @param title title
#' @param col color set
#' @param nrow show lengend row
#' @param width legend width
#' @param height legend height
#' @param title_font title font size
#' @param label_font label font size
#'
#' @export
#'

Customize_LegendParam <- function(
            title=NULL,
            col=NULL,
            nrow=3,
            width=2,
            height=2,
            title_font=6,
            label_font=5
){
    lgd <- Legend(title=title, 
                labels=names(col), 
                legend_gp=gpar(fill=col), 
                nrow=nrow, 
                title_gp=gpar(fontsize=title_font), 
                labels_gp=gpar(fontsize=label_font), 
                grid_width=unit(width, "mm"), 
                grid_height=unit(height, "mm"))

    lgd
}




#' ComplexHeatmap for Cell Type as group
#'
#' @param data dataframe
#' @param top_anno1 top annotation group-1
#' @param top_anno2 top annotation group-2
#' @param colors color set
#' @param col_celltype color group-1
#' @param col_group color group-2
#' @param zscore zscore yes/no
#' @param show_row_names yes/no
#' @param show_column_dend yes/no
#' @param top_annotation show top annotation yes/no
#' @param show_feature show specific gene name or not
#' @param row_split split row
#'
#' @export
#'
ComplexHeatmap_CellType <- function(
    data=NULL, 
    top_anno1=NULL, 
    top_anno2=NULL,
    colors=NULL,
    col_celltype=NULL,
    col_group=NULL,
    zscore=FALSE,
    show_row_names=FALSE,
    show_row_dend=FALSE,
    cluster_rows=FALSE,
    show_column_names=FALSE,
    show_column_dend=TRUE,
    cluster_columns=TRUE,
    top_annotation=TRUE,
    show_feature=NULL,
    use_raster=FALSE,
    row_split=NULL,
    font_size=6,
    label_font=5
){

    info <- data[,c(top_anno1, top_anno2)]
    gene_names <- colnames(data)
    features <- gene_names[!gene_names %in% c(top_anno1, top_anno2)]
    d_mtx <- as.matrix(t(data[, features]))


    if (zscore){
        d_mtx = t(scale(t(d_mtx)))
        #c("#09103b", "#5f79cf", "white", "#eb6565", "#540506")
        d_mtx[!is.na(d_mtx) & d_mtx < -1] <- -1
        d_mtx[!is.na(d_mtx) & d_mtx > 1] <- 1

        colors = circlize::colorRamp2(c(-1, -0.5, 0, 0.5, 1), c("#00A9E0FF", "#CCEEF9FF", "white", "#FFE099FF", "#A50021FF"))
        ht_title = "Row Z-Score"
    }

    
    ha <- NULL
    if (top_annotation){
        ha <- HeatmapAnnotation(
                CellType = anno_simple(x=info[[top_anno1]], simple_anno_size = unit(2, "mm"), col=col_celltype),
                Group = anno_simple(x=info[[top_anno2]], simple_anno_size = unit(2, "mm"), col=col_group),
                annotation_name_gp = gpar(fontsize = font_size)
              )
    }

    feature_anno <- NULL
    if (length(show_feature) > 0) {
        feature_set <- as.data.frame(rownames(d_mtx))
        colnames(feature_set) <- 'gene'
        feature_set$id <- 1:nrow(feature_set)
        index_id <- feature_set[feature_set$gene %in% show_feature,]$id
        feature_anno = rowAnnotation('Gene' = anno_mark(at =index_id, labels = show_feature, labels_gp = gpar(col = "black", fontsize = label_font), link_width = unit(3, "mm")))
    }
    
    
    ht_exp <- Heatmap(
                d_mtx,

                col = colors,
                    
                show_row_names = show_row_names,
                row_names_side = "left",
                row_names_gp = gpar(fontsize = font_size),
                show_row_dend = show_row_dend,
                cluster_rows = cluster_rows,

                show_column_names = show_column_names,
                column_names_rot = 60,
                column_names_gp = gpar(fontsize = font_size),
                show_column_dend = show_column_dend,
                cluster_columns = cluster_columns,

                # split row
                row_split = row_split,
                row_gap = unit(0.3, "mm"),
                row_title_gp = gpar(fontsize = font_size),
                
                # split column
                column_split = factor(info[[top_anno1]], levels = unique(info[[top_anno1]])),
                cluster_column_slices = FALSE,
                column_gap = unit(0.3, "mm"),
                column_title_gp = gpar(fontsize = 0), 
                use_raster = use_raster,
                
                # show anno group
                top_annotation = ha,
                right_annotation= feature_anno,

                # legend
                heatmap_legend_param = list(
                        title = 'Expression',
                        direction = "horizontal",
                        title_position = "lefttop",
                        title_gp = gpar(fontsize = font_size), 
                        labels_gp = gpar(fontsize = label_font),
                        grid_width = unit(2, "mm"),
                        grid_height = unit(2, "mm")
                    )
            )

    return(ht_exp)
}




