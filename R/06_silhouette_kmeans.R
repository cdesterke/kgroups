#' Detailed silhouette plot for k-means clustering
#'
#' This function computes the silhouette values for a k-means clustering result
#' and returns a ggplot2 silhouette plot with customizable palette and base font size.
#'
#' @param mat A numeric matrix or data frame (samples in rows).
#' @param clusters A vector of cluster assignments (same order as rownames(mat)).
#' @param palette A vector of colors or a palette name (default: "Dark2").
#' @param base_size Base font size for the plot (default: 14).
#' @param main Title of the silhouette plot.
#'
#' @return A ggplot2 object representing the silhouette plot.
#'
#' @importFrom stats dist
#' @importFrom cluster silhouette
#' @importFrom factoextra fviz_silhouette
#'
#' @examples
#' \dontrun{
#'   km <- kmeans(mat_scaled, centers = 3, nstart = 50)
#'   silhouette_kmeans(mat_scaled, km$cluster, palette = "Set2", base_size = 16)
#' }
#'
#' @export
silhouette_kmeans <- function(mat,
                              clusters,
                              palette = "Dark2",
                              base_size = 14,
                              main = NULL) {
  
  stopifnot(is.matrix(mat) || is.data.frame(mat))
  stopifnot(length(clusters) == nrow(mat))
  
  if (is.null(main)) {
    main <- paste("Silhouette detailed (k =", length(unique(clusters)), ")")
  }
  
  # Compute silhouette
  sil_obj <- cluster::silhouette(clusters, stats::dist(mat))
  
  # Plot without sample labels AND without x-axis labels
  p <- factoextra::fviz_silhouette(
    sil_obj,
    palette = palette,
    label = FALSE
  ) +
    ggplot2::ggtitle(main) +
    ggplot2::theme_classic(base_size = base_size) +
    
    # ---- FIX: axe X discret → scale_x_discrete ----
    ggplot2::scale_x_discrete(breaks = NULL, labels = NULL) +
    
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  
  return(p)
}
