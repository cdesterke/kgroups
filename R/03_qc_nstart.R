#' Assess k-means stability across different nstart values
#'
#' This function evaluates the stability of k-means clustering by running
#' multiple initializations (`nstart`) and comparing the resulting
#' total within-cluster sum of squares (`tot.withinss`). It produces a
#' boxplot showing how sensitive the clustering is to initialization.
#'
#' @param mat A numeric matrix or data frame (samples in rows).
#' @param k Number of clusters for k-means (default: 2).
#' @param nstart_values A numeric vector of nstart values to test.
#' @param replicates Number of repeated k-means runs per nstart value (default: 20).
#'
#' @return A ggplot2 boxplot object showing tot.withinss distribution per nstart.
#'
#' @importFrom stats kmeans
#' @importFrom ggplot2 ggplot aes geom_boxplot labs theme_minimal
#'
#' @examples
#' \dontrun{
#'   mat_scaled <- scale(t(mat))
#'   qc_nstart(mat_scaled, k = 3)
#' }
#'
#' @export
qc_nstart <- function(mat,
                      k = 2,
                      nstart_values = c(1, 5, 10, 20, 50),
                      replicates = 20) {
  
  # Vérifications CRAN-friendly
  stopifnot(is.matrix(mat) || is.data.frame(mat))
  stopifnot(k >= 2)
  stopifnot(all(nstart_values >= 1))
  stopifnot(replicates >= 1)
  
  # Construction du tableau de résultats
  df <- do.call(rbind, lapply(nstart_values, function(ns) {
    data.frame(
      nstart = ns,
      tot_withinss = replicate(
        replicates,
        stats::kmeans(mat, centers = k, nstart = ns)$tot.withinss
      )
    )
  }))
  
  # Plot CRAN-safe (pas de print)
  ggplot2::ggplot(df, ggplot2::aes(factor(nstart), tot_withinss)) +
    ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.6) +
    ggplot2::labs(
      x = "nstart",
      y = "Total within-cluster sum of squares",
      title = paste("k-means nstart stability (k =", k, ")")
    ) +
    ggplot2::theme_minimal()
}
