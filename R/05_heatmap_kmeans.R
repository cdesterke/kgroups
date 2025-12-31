#' Heatmap annotated with k-means clusters
#'
#' This function generates a heatmap of a scaled matrix with samples annotated
#' by their k-means cluster assignment. The user can customize the color palette
#' and the base font size.
#'
#' @param mat A numeric matrix or data frame (samples in rows).
#' @param clusters A vector of cluster assignments (same order as rownames(mat)).
#' @param palette A vector of colors or a palette name (default: NULL → automatic).
#' @param base_size Base font size for the heatmap (default: 12).
#' @param main Title of the heatmap.
#'
#' @return A pheatmap object.
#'
#' @importFrom pheatmap pheatmap
#'
#' @examples
#' \dontrun{
#'   mat_scaled <- scale(t(mat))
#'   res <- run_kmeans_with_pca(mat_scaled, k = 3)
#'   heatmap_kmeans(mat_scaled, res$clusters)
#' }
#'
#' @export
heatmap_kmeans <- function(mat,
                           clusters,
                           palette = NULL,
                           base_size = 12,
                           main = "Heatmap + clusters") {
  
  stopifnot(is.matrix(mat) || is.data.frame(mat))
  stopifnot(length(clusters) == nrow(mat))
  
  # ------------------------------------------------------------
  # Annotation dataframe
  # ------------------------------------------------------------
  annotation <- data.frame(Cluster = factor(clusters))
  rownames(annotation) <- rownames(mat)
  
  # ------------------------------------------------------------
  # Palette automatique si non fournie
  # ------------------------------------------------------------
  k <- length(unique(annotation$Cluster))
  
  if (is.null(palette)) {
    # palette qualitative automatique
    palette <- grDevices::rainbow(k)
  }
  
  ann_colors <- list(
    Cluster = stats::setNames(palette[1:k], levels(annotation$Cluster))
  )
  
  # ------------------------------------------------------------
  # Heatmap
  # ------------------------------------------------------------
  p <- pheatmap::pheatmap(
    t(mat),
    annotation_col = annotation,
    annotation_colors = ann_colors,
    show_rownames = FALSE,
    show_colnames = FALSE,
    fontsize = base_size,
    main = main,
    border_color = NA
  )
  
  return(p)
}
