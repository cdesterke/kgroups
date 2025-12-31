#' Run k-means clustering and visualize PCA with customizable palette and base size
#'
#' This function performs k-means clustering on a scaled matrix and returns
#' both the cluster assignments and a PCA visualization colored by clusters.
#' The user can customize the color palette and the base font size of the plot.
#'
#' @param mat A numeric matrix or data frame (samples in rows).
#' @param k Number of clusters for k-means.
#' @param nstart Number of random initializations for k-means (default: 50).
#' @param seed Random seed for reproducibility (default: 123).
#' @param palette A color palette name (e.g. "Dark2", "Set1") or a vector of colors.
#' @param base_size Base font size for the PCA plot (default: 14).
#'
#' @return A list containing:
#' \describe{
#'   \item{clusters}{Cluster assignments (vector).}
#'   \item{kmeans_model}{The k-means model object.}
#'   \item{pca_plot}{A ggplot2 PCA plot colored by clusters.}
#' }
#'
#' @importFrom stats kmeans prcomp
#' @importFrom factoextra fviz_pca_ind
#'
#' @examples
#' \dontrun{
#'   mat_scaled <- scale(t(mat))
#'   res <- run_kmeans_with_pca(mat_scaled, k = 3, palette = "Set1", base_size = 16)
#'   res$pca_plot
#'   res$clusters
#' }
#'
#' @export
run_kmeans_with_pca <- function(mat,
                                k,
                                nstart = 50,
                                seed = 123,
                                palette = "Dark2",
                                base_size = 14) {
  
  stopifnot(is.matrix(mat) || is.data.frame(mat))
  stopifnot(k >= 2)
  stopifnot(base_size > 0)
  
  # ------------------------------------------------------------
  # K-means clustering
  # ------------------------------------------------------------
  set.seed(seed)
  km <- stats::kmeans(mat, centers = k, nstart = nstart)
  clusters <- km$cluster
  
  # ------------------------------------------------------------
  # PCA + clusters
  # ------------------------------------------------------------
  pca_plot <- factoextra::fviz_pca_ind(
    stats::prcomp(mat),
    geom = "point",
    habillage = clusters,
    palette = palette,
    addEllipses = TRUE,
    title = paste("PCA (k-means clustering, k =", k, ")")
  ) +
    ggplot2::theme_minimal(base_size = base_size)
  
  # ------------------------------------------------------------
  # Retour
  # ------------------------------------------------------------
  list(
    clusters = clusters,
    kmeans_model = km,
    pca_plot = pca_plot
  )
}
